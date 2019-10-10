run_experiment <- function(run_id, 
                           config_object, 
                           k = 10, 
                           run_pitch_to_coefs = FALSE,
                           run_coefs_to_clusters = FALSE) {
 
   print(paste("Initiating experiment", run_id, "with", k, "folds"))
  
  if (run_pitch_to_coefs) {
    print("Generating new poly coefs from pitch contours")
    d_by_bin <- pitch_to_coefs(run_id, config_object)
  } else {
    print("Loading stored poly coefs")
    d_by_bin <- read_rds(here(paste0(config_object$paths_config$pitch_sum_path, 
                                     "lena-pred-nested-pitch-vals-", 
                                     config_object$exp_config$dataset_name, 
                                     "-",
                                     config_object$kmeans_config$scale_coefs,
                                     ".rds"))) 
  }
  
  # only generate a new clusters dataset if the user wants to
  if (run_coefs_to_clusters) {
    print("Generating new cluster dataset from poly coefs")
    d_clusters <- generate_cluster_dataset(d_by_bin, run_id, config_object)
  } else {
    print("Loading stored cluster dataset")
    d_clusters <- read_rds(path = here(paste0(config_object$paths_config$pitch_sum_path, 
                                              "lena-pred-clustering-outputs-", 
                                              config_object$exp_config$dataset_name, 
                                              "-",
                                              config_object$kmeans_config$scale_coefs,
                                              ".rds")))
  }
  
  # sample one set of test data to be used across all the different training sets
  one_clusters_dataset <- purrr::pluck(d_clusters, 1, "d_clusters")
  
  # k-fold cross validation to create a variety of test sets
  k_fold_test_sets <- run_k_fold(one_clusters_dataset, k = k, config_object$dnn_dataset_config)
  
  # for each fold of test data, we now generate a dataset, train model, and generate predictions
  d_results <- vector(mode="list", length=length(1:k))
  names(d_results) <- paste0("fold", as.character(1:k))
  counter <- 1
  
  for (test_data in k_fold_test_sets) {
    fold_id <- paste0("fold", as.character(counter))
    print(paste("Running:", fold_id))
    
    # generate a dnn-ready dataset (vectorize subsequences)
    d_dnn <- generate_dnn_dataset(d_clusters, 
                                  config_object, 
                                  run_id, 
                                  test_data = test_data, 
                                  fold = test_data$fold_id[1],
                                  n_folds = k)
    
    # train dnn and generate predictions on test data, adding results to list
    # we name the result after the fold id
    d_results[[fold_id]] <- fit_dnn(d_dnn,
                                    config_object, 
                                    run_id, 
                                    fold = test_data$fold_id[1])
    counter <- counter + 1
  }
  
  # save runs separately
  # write the results to disk
  write_rds(d_results,
            here(config_obj$paths_config$lstm_preds_path, 
                 paste0("lena-pred-lstm-preds-",
                        config_obj$exp_config$dataset_name, "-",
                        config_obj$kmeans_config$scale_coefs, "-",
                        run_id,
                        ".rds")),
            compress = "gz")
  
  print(paste("Completed and saved result for experimental:", run_id))
}

pitch_to_coefs <- function(run_id, config_object) {
  # Read raw pitch values ---------------------------------------------------
  d <- read_rds(here(config_object$paths_config$pitch_sum_path, 
                     paste0("lena-pred-pitch-vals-pre-interp-", 
                            config_object$exp_config$dataset_name,
                            ".rds")))
  
  d <- d %>% mutate(exp_run_id = run_id)
  
  # extract speaker id from segment id if we are using IDSLabel data
  # this is some regex magic I figured out through trial and error 
  # but basically we are matching any character before the first "_" in seg id
  # and using the non-greedy "?" operator to only extract from the first "_"
  if (config_object$exp_config$dataset_name == "IDSLabel") {
    d <- d %>% extract(col = seg_id, "speaker_id", regex = "(.*?(?=_))",
                       remove = FALSE)
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
  
  # Fit polynomial
  d_by_bin <- d_by_bin %>%
    mutate(poly_coefs = map(data, fit_poly, config_object$poly_fit_config$degree_poly)) %>%
    mutate(poly_preds = map(poly_coefs, predict_poly))
  
  # Save nested output
  write_rds(d_by_bin, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-nested-pitch-vals-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  print(paste0("Completed fitting polynomial to each time bin for: ", run_id))
  
  d_by_bin
}

## note that saving functions will create output with each run of the experiment
## so we store only the intermediate outputs of the final run of the experiment 
generate_cluster_dataset <- function(d_by_bin, run_id, config_object) {
  
  # Kmeans clustering of poly coefs -----------------------------------------
  d_coefs <- unnest(d_by_bin, poly_coefs) %>% 
    ungroup() %>% 
    select(-data, -poly_preds)
  
  d_final <- map_get_clusters(d = d_coefs, 
                              k_list = config_object$poly_fit_config$n_q_shapes,
                              run_id = run_id,
                              iter_max = config_object$kmeans_config$iter_max, 
                              scale_coefs = config_object$kmeans_config$scale_coefs) 
  
  # Save kmeans output -----------------------------------------
  write_rds(d_coefs, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-kmeans-outputs-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  # Save clusters output -----------------------------------------
  write_rds(d_final, 
            here(paste0(config_object$paths_config$pitch_sum_path, 
                        "lena-pred-clustering-outputs-", 
                        config_object$exp_config$dataset_name, 
                        "-",
                        config_object$kmeans_config$scale_coefs,
                        ".rds")), compress = "gz")
  
  
  print(paste("Completed clustering for:", run_id))
  
  d_final
}

generate_dnn_dataset <- function(d_clusters, config_object, run_id, test_data_gen, fold, n_folds) {
  d_list_clusters <- d_clusters %>% map(~ .x$d_clusters)
  
  d_lstm <- furrr::future_map(d_list_clusters,
                names(d_list_clusters),
                .f = build_lstm_datasets, 
                prop_cds_train_list = config_object$dnn_dataset_config$prop_train_cds,
                dnn_config = config_object$dnn_dataset_config,  
                test_data = test_data_gen)
  

   # Save final lstm dataset
  if (fold == n_folds) {
    write_rds(d_lstm,
              here(config_object$paths_config$lstm_sum_path,
                   paste0("lena-pred-lstm-train-test-",
                          config_object$exp_config$dataset_name,
                          "-",
                          config_object$kmeans_config$scale_coefs,
                          ".rds")),
              compress = "gz")
  }
  
  print(paste("Completed DNN dataset generation for", run_id, ", fold", fold))
  
  d_lstm
}

fit_dnn <- function(d_dnn, config_object, run_id, fold) {
  d_input <- d_dnn %>% flatten() 
  # create list of models for each dataset: prop CDS and n-qshapes
  mods <- d_input %>% map(create_lstm, lstm_config = config_object$lstm_config)
  
  # train model and generate predictions 
  # the train lstm function also handles post-processing 
  # and tidying the model predictions
  results_obj <- pmap(list(mods, names(mods), d_input), 
                      .f = safe_train_lstm, 
                      lstm_config = config_object$lstm_config)
  
  print(paste0("Completed DNN training and prediction for ", run_id, " and fold", fold))
  
  results_obj
}
