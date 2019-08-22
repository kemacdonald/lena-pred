run_experiment <- function(run_id, config_object) {
  # build sequence of cluters data set
  d_clusters <- generate_cluster_dataset(run_id, config_object)
  
  # sample one set of test data to be used across all the different training sets
  one_clusters_dataset <- purrr::pluck(d_clusters, 1, "d_clusters")

  d_test <- get_test_data(one_clusters_dataset,
                          dnn_config = config_object$dnn_dataset_config,
                          run_id)
  
  # generate a dnn-ready dataset (vectorize subsequences)
  d_dnn <- generate_dnn_dataset(d_clusters, config_object, run_id, test_data = d_test)

  # train dnn and generate predictions on test data
  d_results <- fit_dnn(d_dnn, config_object, run_id)

  print(paste("Completed:", run_id))

  d_results
}

## note that saving functions will create output with each run of the experiment
## so we store only the intermediate outputs of the final run of the experiment 
generate_cluster_dataset <- function(run_id, config_object) {
  # Read raw pitch values ---------------------------------------------------
  d <- read_rds(here(config_object$paths_config$pitch_sum_path, 
                     paste0("lena-pred-pitch-vals-pre-interp-", 
                            config_object$exp_config$dataset_name,
                            ".rds")))
  
  d <- d %>% mutate(exp_run_id = run_id)
  
  if (config_object$exp_config$dataset_name == "IDSLabel") {
    d <- d %>% mutate(speaker_id = str_split(seg_id, "_", simplify = TRUE)[1])
  }
  
  # Batch interpolate  ------------------------------------------------------
  d_interp <- batch_interpolate(d, config_object$loess_config) 
  # scale the log interpolated pitch values
  d_interp <- d_interp %>% mutate(z_log_pitch_interp = scale(log_pitch_interp)) 
  
  # Temporal segmentation  --------------------------------------------------
  d_interp <- d_interp %>%
    create_time_bins(bin_width = config_object$time_filter_config$time_bin_width) %>%
    get_time_in_bin(sample_rate = config_object$loess_config$preds_sample_rate) %>%
    relabel_bins()
  
  # Remove 100 ms segments with fewer than the min number of samples in each bin
  d_interp <- d_interp %>% filter(n_bins_in_seg == config_object$time_filter_config$min_samples_bin)
  
  # Fix issue with speaker 126 -> should be 1266
  # And add the duration of segments in ms for clip
  d_interp <- d_interp %>% 
    mutate(speaker_id = ifelse(speaker_id == "126", "1266", speaker_id)) %>%
    group_by(seg_id) %>% 
    mutate(duration_ms = max(time))
  
  # Save intermediate output
  write_rds(d_interp, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-pitch-vals-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  print(paste0("Completed loess fit for: ", run_id))
  
  # Fit second-order polynomial for each time bin ----------------------------
  d_by_bin <- d_interp %>%
    group_by(seg_id, dataset, speech_register, speaker_id,
             time_bin_id, duration_ms, speaker_id, exp_run_id) %>%
    nest() 
  
  # Save nested output
  write_rds(d_by_bin, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-nested-pitch-vals-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  # Fit polynomial
  d_by_bin <- d_by_bin %>%
    mutate(poly_coefs = future_map(data, fit_poly, config_object$poly_fit_config$degree_poly)) %>%
    mutate(poly_preds = future_map(poly_coefs, predict_poly))
  
  # Kmeans clustering of poly coefs -----------------------------------------
  d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)
  d_final <- map_get_clusters(config_object$poly_fit_config$n_q_shapes,
                              run_id = run_id,
                              d = d_coefs, 
                              iter_max = config_object$kmeans_config$iter_max, 
                              scale_coefs = config_object$kmeans_config$scale_coefs) 
  
  # Save kmeans output
  write_rds(d_coefs, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-kmeans-outputs-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  
  print(paste0("Completed clustering for: ", run_id))
  
  d_final
}

get_test_data <- function(d, dnn_config, run_id) {
  # get the number of segements to sample from each speaker for ADS and IDS
  d_seg_ids <- d %>% distinct(seg_id, speech_register) 
  n_total <- nrow(d_seg_ids)
  n_speakers <- d %>% distinct(speaker_id) %>% nrow()
  
  # get the number of training vs. test segments 
  n_to_sample_train <- as.integer(n_total * dnn_config$prop_train)
  n_to_sample_test <- n_total - n_to_sample_train
  
  # get the number of segs to sample from each  speaker
  n_per_speech_reg_test <- ceiling( (n_to_sample_test * dnn_config$prop_test_cds) / n_speakers )
  
  # get seg ids for test data and add a sample id to keep track 
  test_seg_ids <- d %>%
    distinct(seg_id, speech_register, speaker_id) %>%
    group_by(speech_register, speaker_id) %>%
    sample_n(n_per_speech_reg_test, replace = FALSE) %>%
    ungroup() %>%
    select(seg_id) %>%
    mutate(sample_id = 1:n())
  
  print(paste0("Completed sampling test data for: ", run_id))
  
  d %>%
    filter(seg_id %in% test_seg_ids$seg_id) %>%
    select(seg_id, speech_register, time_bin_id, duration_ms) %>%
    left_join(., test_seg_ids, by = c("seg_id"))
}

generate_dnn_dataset <- function(d_clusters, config_object, run_id, test_data_gen) {
  d_list_clusters <- d_clusters %>% future_map(~ .x$d_clusters)
  
  d_lstm <- future_map(d_list_clusters,
                       names(d_list_clusters),
                       .f = build_lstm_datasets, 
                       prop_cds_train_list = config_object$dnn_dataset_config$prop_train_cds,
                       dnn_config = config_object$dnn_dataset_config,  
                       test_data = test_data_gen)
  
  # Save final lstm dataset
  write_rds(d_lstm, 
            here(config_obj$paths_config$lstm_sum_path, 
                 paste0("lena-pred-lstm-train-test-",
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), 
            compress = "gz")
  
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
                      lstm_config = config_object$lstm_config)
  
  print(paste0("Completed DNN training and prediction for: ", run_id))
  
  results_obj
}