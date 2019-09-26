# Persistence model -------------------------------------------------------
# The persistence algorithm uses the value at the previous 
# time step (t-1) to predict the expected outcome at the next 
# time step (t+1).

# TODO fix model train/evaluate pipeline to save at least one instance of all q-shapes and prop_cds

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))

d_list <- read_rds(here(config_obj$paths_config$lstm_sum_path, 
                        paste0("lena-pred-lstm-train-test-", 
                               config_obj$exp_config$dataset_name,
                               "-",
                               config_obj$kmeans_config$scale_coefs,
                               ".rds"))) 

d_flat <- flatten(d_list) 

d_results <- map2_df(d_flat, names(d_flat), 
                     .f = score_persistence_alg)

write_csv(d_results, here(config_obj$paths_config$lstm_sum_path, 
                          paste0("lena-pred-persist-results-",
                                 config_obj$exp_config$dataset_name,
                                 "-",
                                 config_obj$kmeans_config$scale_coefs,
                                 ".csv")))
          