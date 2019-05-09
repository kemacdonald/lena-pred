######### Extract pitch tracks in batch ###########

### Libraries and Helpers -----------------------------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch_helpers.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))

### Extraction parameters from config file -----------------------------------------------
source(here("code/00_config/lena-pred-pitch-config.R"))

### Extract pitch tracks -------------------------------------------------------------------
d <- dataset_config$files_to_analyze[1:40] %>% map_df(get_pitch_contour, pitch_detect_config)

### Filter to first and last voiced frames 
batch_filter_voiced(d) -> d

# log transform pitch data
d %>% mutate(log_pitch = log(pitch)) -> d

# Create a blacklist of segment ids with too few pitch estimate 
# to do reliable loess interpolation 
seg_id_blacklist <- flag_too_few_pitch(d, loess_config$min_n_samples_loess) 

### Batch interpolate -------------------------------------------------------------------
batch_interpolate(d, loess_config, seg_id_blacklist) -> d_interp  

# scale the log interpolated pitch values
d_interp %>% mutate(z_log_pitch_interp = scale(log_pitch_interp)) -> d_interp

# ### Temporal segmentation -------------------------------------------------------------------
# Divide each audio clip into fixed frame 100ms segments.
d_interp %>%
  create_time_bins(bin_width = time_filter_config$time_bin_width) %>%
  get_time_in_bin(sample_rate = loess_config$preds_sample_rate) %>%
  relabel_bins() -> d_interp

# Remove 100 ms segments with fewer than the min number of samples in each bin.
d_interp %>% filter(n_bins_in_seg == time_filter_config$min_samples_bin) -> d_interp

### Fit poly and cluster -------------------------------------------------------------------
# Fit second-order polynomial in each time bin
d_interp %>%
  group_by(seg_id, dataset, speech_register, time_bin_id) %>%
  nest() -> d_by_bin

# fit polynomial and make predictions based on fit
d_by_bin %>%
  mutate(poly_coefs = map(data, fit_poly, poly_fit_config$degree_poly)) %>%
  mutate(poly_preds = map(poly_coefs, predict_poly)) -> d_by_bin

# Kmeans clustering of poly coefs
d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)
d_coefs %>% get_cluster_assignments(k = poly_fit_config$n_q_shapes, scale_coefs = T) -> d_final

# add cluster assignments to data
d_by_bin %>% left_join(select(d_final$d_clusters, seg_id, time_bin_id, cluster)) -> d_by_bin

# Add the number of segments (q-shapes in each clip)
add_utt_duration_segs(d_final$d_clusters) -> d_final$d_clusters

### Generate train and test data from DNN ---------------------------------------------------
d_lstm <- generate_lstm_dataset(d_final$d_clusters,
                                max_seq_len = dnn_dataset_config$seq_max_len,
                                skip = dnn_dataset_config$skip_val,
                                train_test_split = dnn_dataset_config$prop_train,
                                prop_cds = dnn_dataset_config$prop_train_cds)

##### Save outputs ----------------------------------------------------------------------------
write_path <- paste0("data/03_summaries/", data_set)
write(seg_id_blacklist, here(write_path, "lena-pred-seg-blacklist.txt"))
write_fst(d_interp, here(write_path, "lena-pred-pitch-vals.fst"))
write_fst(d_final$centers, here(write_path, "lena-pred-kmeans-centers.fst"))
write_fst(d_final$d_clusters, here(write_path, "lena-pred-poly-coefs.fst"))
write_rds(d_by_bin, here(write_path, "lena-pred-nested-pitch-vals.rds"))
write_rds(d_lstm, here(write_path, "lena-pred-lstm-train-test.rds"))