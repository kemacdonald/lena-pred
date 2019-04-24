######### Extract pitch tracks in batch

##### Libraries ----------------------------------------------------------------------------------
library(soundgen); library(tidyverse); library(here)
source(here("code/00_helper_functions/pitch_helpers.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))

##### Extraction parameters  ---------------------------------------------------------------------
set.seed(12345)
path_to_wav <- "data/02_processed_data/pilot-segments-norm"
min_prop_voiced <- 0.1 # min proportion of voiced samples (need to ask AW about this number)
time_bin_width <- 100  # in ms

pitch_min <- 75             # clips extreme pitch estimates
pitch_max <- 750            # clips extreme pitch estimates 
silence_min <- 0.01         # lower bound of silence in recording, silent frames are not analyzed 
ent_threshold <- 0.6
autocor_thresh <- 0.7
pathfinding_alg <- "fast"   # method of finding the optimal path through pitch candidates
pitch_methods = c("autocor", "spec", "dom")

min_n_samples_loess <- 20   # min number of pitch candidates needed to fit loess (20 = 500 ms)
frac_points_loess <- 0.20   # what percentage point for fitting loess (higher = less wiggly)
preds_sample_rate <- 10    # how frequently to sample from fitted loess (ms)

min_samples_bin <- 10  # min number of samples in a time bin
degree_poly <- 2 # degree of polynomial curve fit to the temporal segments
n_q_shapes <- 10  # number of q-shapes for k-means clustering step

seq_max_len <- 10     # max length of the sub-sequences for training lstm
skip_val <- 1         # how many steps to shift each training sub-sequence
prop_train <- 0.9     # prop of uttereances to use for training vs. test
prop_train_cds <- 0.5 # prop of CDS utterances in th training dataseet

# path to audio
files_to_analyze <- list.files(here(path_to_wav), pattern = "*.wav", recursive = T)

# add the full path
files_to_analyze <- here(path_to_wav, files_to_analyze)

##### Main script ----------------------------------------------------------------------------------

### Extract pitch tracks ###
d <- files_to_analyze %>% map_df(get_pitch_contour, 
                                 pitchFloor = pitch_min, pitchCeiling = pitch_max,
                                 pitchMethods = pitch_methods, pathfinding = pathfinding_alg, 
                                 silence = silence_min,entropyThres = ent_threshold,
                                 autocorThres = autocor_thresh)

### Filter to first and last voiced frames ###
batch_filter_voiced(d) -> d

# Create a blacklist of segment ids with too few pitch estimate to do reliable 
# loess interpolation 
flag_too_few_pitch(d, min_n_samples_loess) -> seg_id_blacklist 

# Interpolate pitch candidates across unvoiced regions of signal using loess 
batch_interpolate(d) -> d_interp  

# log transform and Z-score interpolated pitch values
d_interp %>% mutate(log_pitch = log(pitch_interpolated), z_log_pitch = scale(log_pitch)) -> d_interp

### Divide each audio clip into fixed frame 100ms segments.
d_interp %>% 
  create_time_bins(bin_width = time_bin_width) %>% 
  get_time_in_bin() %>% 
  relabel_bins -> d_interp

### Remove 100 ms segments with fewer than the min number of samples in each bin
d_interp %>% filter(n_bins_in_seg == min_samples_bin) -> d_interp

### Fit second-order polynomial in each time bin

d_interp %>% 
  group_by(seg_id, dataset, speech_register, time_bin_id) %>% 
  nest() -> d_by_bin

d_by_bin %>% 
  mutate(poly_coefs = map(data, fit_poly, degree_poly)) %>% 
  mutate(poly_preds = map(poly_coefs, predict_poly)) -> d_by_bin

### Kmeans clustering of poly coefs ###
unnest(d_by_bin, poly_coefs, .drop = T) -> d_coefs 

d_coefs %>% get_cluster_assignments(k = n_q_shapes, scale_coefs = T) -> d_final

### Add the number of segments (q-shapes in each clip) ###
add_utt_duration_segs(d_final$d_clusters) -> d_final$d_clusters

### Generate train and test data from DNN ###
d_lstm <- generate_lstm_dataset(d_final$d_clusters, 
                                max_seq_len = seq_max_len, 
                                skip = skip_val,
                                train_test_split = prop_train,
                                prop_cds = prop_train_cds)

##### Save outputs ----------------------------------------------------------------------------------
fst::write_fst(d_final$centers, here("data/03_summaries/lena-pred-kmeans-centers.fst"))
fst::write_fst(d_final$d_clusters, here("data/03_summaries/lena-pred-poly-coefs.fst"))
write_rds(d_lstm, here("data/03_summaries/lena-pred-lstm-train-test.rds"))
