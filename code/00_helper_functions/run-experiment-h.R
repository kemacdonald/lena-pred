run_experiment <- function(run_id, config_object) {
  d_clusters <- generate_cluster_dataset(run_id, config_object)
  d_dnn <- generate_dnn_dataset(d_clusters, config_object, run_id)
  d_results <- fit_dnn(d_dnn, config_object, run_id)
  
  print(paste("Completed", run_id))
  
  d_results
}

generate_cluster_dataset <- function(run_id, config_object) {
  # Read raw pitch values ---------------------------------------------------
  d <- read_rds(here(config_object$paths_config$pitch_sum_path, "lena-pred-pitch-vals-pre-interp.rds"))
  d <- d %>% mutate(exp_run_id = run_id)
  
  # Batch interpolate  ------------------------------------------------------
  d_interp <- batch_interpolate(d, config_object$loess_config) 
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
  # write_rds(d_interp, here(config_obj$paths_config$pitch_sum_path, "lena-pred-pitch-vals.rds"))
  
  print(paste0("Completed loess fit for: ", run_id))
  
  # Fit second-order polynomial in each time bin ----------------------------
  d_by_bin <- d_interp %>%
    group_by(seg_id, dataset, speech_register, speaker_id,
             time_bin_id, duration_ms, speaker_id, exp_run_id) %>%
    nest() 
  
  #  fit polynomial and make predictions based on fit -----------------------
  d_by_bin <- d_by_bin %>%
    mutate(poly_coefs = future_map(data, fit_poly, config_object$poly_fit_config$degree_poly)) %>%
    mutate(poly_preds = future_map(poly_coefs, predict_poly))
  
  # Kmeans clustering of poly coefs -----------------------------------------
  d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)
  d_final <- map_get_clusters(config_object$poly_fit_config$n_q_shapes,
                              run_id = 1,
                              d = d_coefs, 
                              iter_max = config_object$kmeans_config$iter_max, 
                              scale_coefs = config_object$kmeans_config$scale_coefs) 
  
  print(paste0("Completed clustering for: ", run_id))
  
  d_final
}

generate_dnn_dataset <- function(d_clusters, config_object, run_id) {
  d_list_clusters <- d_clusters %>% future_map(~ .x$d_clusters)
  
  d_lstm <- future_map(d_list_clusters,
                       names(d_list_clusters),
                       .f = build_lstm_datasets, 
                       prop_cds_train_list = config_object$dnn_dataset_config$prop_train_cds,
                       dnn_config = config_object$dnn_dataset_config)
  
  print(paste0("Completed DNN dataset generation for: ", run_id))
  
  d_lstm
}

fit_dnn <- function(d_dnn, config_object, run_id) {
  d_input <- d_dnn %>% flatten() 
  # create list of models for each dataset: prop CDS and n-qshapes
  mods <- d_input %>% map(create_lstm, lstm_config = config_object$lstm_config)
  
  # train model and generate predictions 
  # the train lstm function also handles post-processing 
  # and tidying the model predictions
  results_obj <- pmap(list(mods, names(mods), d_input), 
                      .f = safe_train_lstm, 
                      lstm_config = config_object$lstm_config,
                      save_model = FALSE)
  
  print(paste0("Completed DNN training and prediction for: ", run_id))
  
  results_obj
}