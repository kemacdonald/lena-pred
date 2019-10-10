source(here::here("code/00_config/lena-pred-libraries.R")) 
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)

config_object <- config_obj
config_object$dnn_dataset_config$prop_train_cds <- 0.5
k <- 1
run_id <- "run1"
prop_dataset <- 0.4

d_clusters <- read_rds(here(paste0(config_obj$paths_config$pitch_sum_path, 
                           "lena-pred-clustering-outputs-", 
                           config_obj$exp_config$dataset_name, 
                           "-",
                           config_obj$kmeans_config$scale_coefs,
                           ".rds")))


# subsample training data get smaller dataset to debug
seg_ids <- d_clusters[1]$`shapes_6-run1`$d_clusters$seg_id %>% unique()
seg_ids_subset <- sample(seg_ids, size = length(seg_ids) * prop_dataset)

d_clusters_sub <- list(`shapes_6-run1` = list(
  centers = d_clusters[1]$`shapes_6-run1`$centers,
  d_clusters = d_clusters[1]$`shapes_6-run1`$d_clusters %>% filter(seg_id %in% seg_ids_subset) 
))

# sample one set of test data to be used across all the different training sets
one_clusters_dataset <- purrr::pluck(d_clusters, 1, "d_clusters")

# k-fold cross validation to create a variety of test sets
k_fold_test_sets <- run_k_fold(one_clusters_dataset, k = k, config_obj$dnn_dataset_config)
d_results <- vector(mode="list", length=length(1:k))
names(d_results) <- paste0("fold", as.character(1:k))
counter <- 1

for (test_data in k_fold_test_sets) {
  fold_id <- paste0("fold", as.character(counter))
  print(paste("Running:", fold_id))
  
  # generate a dnn-ready dataset (vectorize subsequences)
  d_dnn <- generate_dnn_dataset(d_clusters_sub, 
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

# train dnn and generate predictions on test data, adding results to list
# we name the result after the fold id

lstm_config = list(
  lstm_units = 240,
  lstm_output_dim = 30,
  n_epochs = 30,
  include_early_stop = FALSE, # set to FALSE if we want the same number of training epochs across "conditions"
  early_stop = callback_early_stopping(monitor = "val_loss", 
                                       min_delta = 0.0001, patience = 3, 
                                       verbose = 0, mode = "auto"),
  batch_size = 1000, 
  validation_split = 0.1, # 0 means we use all data for training
  dropout = 0,
  lr = .01,
  shuffle = TRUE,
  save_model = FALSE,
  n_clusters = n_qshapes_vals,
  input_shape = n_qshapes_vals + 1
)

config_object$lstm_config <- lstm_config

d_results[[fold_id]] <- fit_dnn(d_dnn,
                                config_object, 
                                run_id, 
                                fold = test_data$fold_id[1])
