# Interpolate pitch and cluster poly coefs --------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch-extraction-h.R"))
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)

# Read raw pitch values ---------------------------------------------------
d <- read_rds(here(config_obj$paths_config$pitch_sum_path, "lena-pred-pitch-vals-pre-interp.rds"))
d <- d %>% mutate(exp_run_id = 1) # for running intermediate version of script

# Batch interpolate  ------------------------------------------------------
d_interp <- batch_interpolate(d, loess_config = config_obj$loess_config)   

# scale the log interpolated pitch values
d_interp <- d_interp %>% mutate(z_log_pitch_interp = scale(log_pitch_interp)) 

# Temporal segmentation  --------------------------------------------------
d_interp <- d_interp %>%
  create_time_bins(bin_width = config_obj$time_filter_config$time_bin_width) %>%
  get_time_in_bin(sample_rate = config_obj$loess_config$preds_sample_rate) %>%
  relabel_bins()

# Remove 100 ms segments with fewer than the min number of samples in each bin
d_interp <- d_interp %>% filter(n_bins_in_seg == config_obj$time_filter_config$min_samples_bin)

# Fix issue with speaker 126 -> should be 1266
# And add the duration of segments in ms for clip
d_interp <- d_interp %>% 
  mutate(speaker_id = ifelse(speaker_id == "126", "1266", speaker_id)) %>%
  group_by(seg_id) %>% 
  mutate(duration_ms = max(time))

# Save intermediate output
write_rds(d_interp, here(config_obj$paths_config$pitch_sum_path, "lena-pred-pitch-vals.rds"))

# Fit second-order polynomial in each time bin ----------------------------
d_by_bin <- d_interp %>%
  group_by(seg_id, dataset, speech_register, speaker_id,
           time_bin_id, duration_ms, speaker_id) %>%
  nest() 

#  fit polynomial and make predictions based on fit -----------------------
d_by_bin <- d_by_bin %>%
  mutate(poly_coefs = future_map(data, fit_poly, config_obj$poly_fit_config$degree_poly)) %>%
  mutate(poly_preds = future_map(poly_coefs, predict_poly)) 

# Kmeans clustering of poly coefs -----------------------------------------
d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)
d_final <- map_get_clusters(config_obj$poly_fit_config$n_q_shapes,
                            run_id = 1,
                            d = d_coefs, 
                            iter_max = config_obj$kmeans_config$iter_max, 
                            scale_coefs = config_obj$kmeans_config$scale_coefs) 

# add one set of cluster assignments to nested data (for viz purposes later on)
d_tmp <- d_final[["shapes_12-run1"]]
d_by_bin <- d_by_bin %>% 
  left_join(select(d_tmp$d_clusters, seg_id, time_bin_id, cluster),
            by = c("seg_id", "time_bin_id")) 

# Save outputs ------------------------------------------------------------
write_rds(d_final, here(config_obj$paths_config$pitch_sum_path, "lena-pred-kmeans-outputs.rds"), compress = "gz")
write_rds(d_by_bin, here(config_obj$paths_config$pitch_sum_path, "lena-pred-nested-pitch-vals.rds"), compress = "gz")

print("Completed polynomial fit and clustering")
