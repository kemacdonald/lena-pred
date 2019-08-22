# Experiment Init ---------------------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)
# runs <- c("run1", "run2", "run3", "run4", "run5")
runs <- c("run1") # for debugging

d_results <- runs %>% 
  map(.f = run_experiment, 
      config_object = config_obj) %>% 
  setNames(runs)

write_rds(d_results,
          here(config_obj$paths_config$lstm_sum_path, 
               paste0("lena-pred-lstm-preds-",
                      config_obj$exp_config$dataset_name,
                      "-",
                      config_obj$kmeans_config$scale_coefs,
                      ".rds")),
          compress = "gz")

beepr::beep(3)

print("Completed and saved all runs of the experiment!")