######### Extract pitch tracks in batch

##### Libraries -----------------------------------------------------
library(soundgen); library(pracma); library(tidyverse); library(here)
source(here("code/00_helper_functions/pitch_helpers.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))

##### Extraction parameters  ----------------------------------------
source(here("code/00_config/lena-pred-pitch-config.R"))

##### Main script ---------------------------------------------------

### Extract pitch tracks
d <- files_to_analyze %>% map_df(get_pitch_contour, pitch_detect_config)

### Filter to first and last voiced frames 
batch_filter_voiced(d) -> d

# Create a blacklist of segment ids with too few pitch estimate to do reliable loess interpolation 
seg_id_blacklist <- flag_too_few_pitch(d, loess_config$min_n_samples_loess) 
batch_interpolate(d, loess_config) -> d_interp  

# log transform and Z-score
d_interp %>% 
  mutate(log_pitch = log(pitch_interpolated), 
         z_log_pitch = scale(log_pitch)) -> d_interp

### Divide each audio clip into fixed frame 100ms segments.
d_interp %>% 
  create_time_bins(bin_width = time_filter_config$time_bin_width) %>% 
  get_time_in_bin(sample_rate = loess_config$preds_sample_rate) %>% 
  relabel_bins() -> d_interp

# Remove 100 ms segments with fewer than the min number of samples in each bin.
d_interp %>% filter(n_bins_in_seg == time_filter_config$min_samples_bin) -> d_interp

### Fit second-order polynomial in each time bin
d_interp %>% 
  group_by(seg_id, dataset, speech_register, time_bin_id) %>% 
  nest() -> d_by_bin

# fit polynomial and make predictions based on fit
d_by_bin %>% 
  mutate(poly_coefs = map(data, fit_poly, poly_fit_config$degree_poly)) %>% 
  mutate(poly_preds = map(poly_coefs, predict_poly)) -> d_by_bin

### Kmeans clustering of poly coefs
d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T) 
d_coefs %>% get_cluster_assignments(k = poly_fit_config$n_q_shapes, scale_coefs = T) -> d_final

# add cluster assignments to data 
d_by_bin %>% left_join(select(d_final$d_clusters, seg_id, time_bin_id, cluster)) -> d_by_bin

### Add the number of segments (q-shapes in each clip
add_utt_duration_segs(d_final$d_clusters) -> d_final$d_clusters

### Generate train and test data from DNN
d_lstm <- generate_lstm_dataset(d_final$d_clusters, 
                                max_seq_len = dnn_dataset_config$seq_max_len, 
                                skip = dnn_dataset_config$skip_val,
                                train_test_split = dnn_dataset_config$prop_train,
                                prop_cds = dnn_dataset_config$prop_train_cds)

##### Save outputs --------------------------------------------------
write_fst(d_interp, here("data/03_summaries/lena-pred-pitch-vals.fst"))
write_fst(d_final$centers, here("data/03_summaries/lena-pred-kmeans-centers.fst"))
write_fst(d_final$d_clusters, here("data/03_summaries/lena-pred-poly-coefs.fst"))
write_rds(d_lstm, here("data/03_summaries/lena-pred-lstm-train-test.rds"))
write_rds(d_by_bin, here("data/03_summaries/lena-pred-nested-pitch-vals.rds"))
