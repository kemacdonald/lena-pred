# Experiment Init ---------------------------------------------------------
source("code/00_config/lena-pred-libraries.R")
source("code/00_config/lena-pred-config.R")
plan(multiprocess)
runs <- c("run1", "run2", "run3", "run4", "run5")
#runs <- c("run1", "run2") # for debugging
n_folds <- 10

# for each fold of test data, we now generate a dataset, train model, and generate predictions
d_results <- vector(mode="list", length=length(1:length(runs)))

# this will return a list of dnn predictions for n_folds datasets
d_results <- runs %>% 
  future_map(run_experiment, 
      config_object = config_obj, 
      k = n_folds,
      generate_clusters = TRUE) %>% 
  setNames(runs)

write_rds(d_results,
          here(config_obj$paths_config$lstm_sum_path, 
          paste0("lena-pred-lstm-preds-",
                 config_obj$exp_config$dataset_name, "-",
                 config_obj$kmeans_config$scale_coefs, ".rds")),
          compress = "gz")

beepr::beep(3)

print("Completed and saved all runs of the experiment!")