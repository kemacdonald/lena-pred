# Persistence model -------------------------------------------------------
# The persistence algorithm uses the value at the previous time step (t-1) to predict 
# the expected outcome at the next time step (t+1).

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))
source(here("code/00_helper_functions/analysis-h.R"))

d_list <- read_rds(here(config_obj$paths_config$lstm_sum_path, 
                        "lena-pred-lstm-train-test.rds")) %>% 
  flatten() 

d_results <- map2_df(d_list, names(d_list), .f = score_persistence_alg)

write_csv(d_results, here(config_obj$paths_config$lstm_sum_path, "lena-pred-persist-results-mb.csv"))
