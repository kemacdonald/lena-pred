run_experiment <- function(n_runs = 5) {
  # generate clusters data set
  d_list <- generate_cluster_dataset(run_id = 1, paths_config, loess_config, time_filter_config, poly_fit_config)
  
  # Generate train and test data for the DNN -------------------------------------------------
  d_list_clusters <- d_list %>% future_map(~ .x$d_clusters)
  
  d_lstm <- future_map(d_list_clusters,
                       names(d_list_clusters),
                       .f = build_lstm_datasets, 
                       prop_cds_train_list = dnn_dataset_config$prop_train_cds,
                       dnn_config = dnn_dataset_config)
  d_lstm
  
  # save results
}




# train and predict from lstm


### Generate train and test data for the DNN -------------------------------------------------
d_lstm <- future_map(d_list_clusters,
                     names(d_list_clusters),
                     .f = build_lstm_datasets, 
                     prop_cds_train_list = dnn_dataset_config$prop_train_cds,
                     dnn_config = dnn_dataset_config)


generate_cluster_dataset <- function(run_id, path_config, loess_config, time_config, poly_config) {
  # Read raw pitch values ---------------------------------------------------
  d <- read_rds(here(path_config$pitch_sum_path, "lena-pred-pitch-vals-pre-interp.rds"))
  d <- d %>% mutate(exp_run_id = run_id)
  
  # Batch interpolate  ------------------------------------------------------
  d_interp <- batch_interpolate(d, loess_config) 
  # scale the log interpolated pitch values
  d_interp <- d_interp %>% mutate(z_log_pitch_interp = scale(log_pitch_interp)) 
  
  # Temporal segmentation  --------------------------------------------------
  d_interp <- d_interp %>%
    create_time_bins(bin_width = time_config$time_bin_width) %>%
    get_time_in_bin(sample_rate = loess_config$preds_sample_rate) %>%
    relabel_bins()
  
  # Remove 100 ms segments with fewer than the min number of samples in each bin
  d_interp <- d_interp %>% filter(n_bins_in_seg == time_config$min_samples_bin)  
  
  # Fix issue with speaker 126 -> should be 1266
  # And add the duration of segments in ms for clip
  d_interp <- d_interp %>% 
    mutate(speaker_id = ifelse(speaker_id == "126", "1266", speaker_id)) %>%
    group_by(seg_id) %>% 
    mutate(duration_ms = max(time))
  
  print(paste0("Completed loess fit for run number: ", run_id))
  
  # Fit second-order polynomial in each time bin ----------------------------
  d_by_bin <- d_interp %>%
    group_by(seg_id, dataset, speech_register, speaker_id,
             time_bin_id, duration_ms, speaker_id, exp_run_id) %>%
    nest() 
  
  #  fit polynomial and make predictions based on fit -----------------------
  d_by_bin <- d_by_bin %>%
    mutate(poly_coefs = future_map(data, fit_poly, poly_config$degree_poly)) %>%
    mutate(poly_preds = future_map(poly_coefs, predict_poly))

  # Kmeans clustering of poly coefs -----------------------------------------
  d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)


  d_final <- map_get_clusters(poly_config$n_q_shapes,
                              run_id = run_id,
                              d = d_coefs,
                              iter_max = 20,
                              scale_coefs = TRUE)

  print(paste0("Completed clustering for run number: ", run_id))

  d_final
}
