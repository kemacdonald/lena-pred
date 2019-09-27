# Experiment Init ---------------------------------------------------------
library(here)
source(here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)
runs <- c("run1", "run2", "run3", "run4", "run5")
n_folds <- 10
# runs <- c("run1", "run2") # for debugging
# n_folds <- 2

# for each fold of test data, we now generate a dataset, train model, and generate predictions
d_results <- vector(mode="list", length=length(1:length(runs)))

# this will return a list of dnn predictions for n_folds datasets
d_results <- runs %>% 
  map(run_experiment, 
      config_object = config_obj, 
      k = n_folds,
      run_pitch_to_coefs = FALSE,
      run_coefs_to_clusters = TRUE) %>% 
  setNames(runs)

# write the results to disk
write_rds(d_results,
          here(config_obj$paths_config$lstm_sum_path, 
          paste0("lena-pred-lstm-preds-",
                 config_obj$exp_config$dataset_name, "-",
                 config_obj$kmeans_config$scale_coefs, ".rds")),
          compress = "gz")

beepr::beep(3)

print("Completed and saved all runs of the experiment!")