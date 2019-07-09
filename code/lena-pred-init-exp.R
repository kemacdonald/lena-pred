# Experiment Init ---------------------------------------------------------

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)
# runs <- c("run1", "run2", "run3", "run4", "run5")
runs <- c("run1")

d_results <- map(runs, 
                 .f = run_experiment, 
                 config_object = config_obj) %>% 
  setNames(runs)

write_rds(d_results,
          here(config_obj$paths_config$lstm_sum_path, "lena-pred-lstm-preds.rds"),
          compress = "gz")

