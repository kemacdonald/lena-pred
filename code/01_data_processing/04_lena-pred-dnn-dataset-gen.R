# Libraries, helpers, and config ------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/lstm-data-gen-h.R"))
source(here("code/00_config/lena-pred-config.R"))
plan(multiprocess)

### Read cluster sequences -----------------------------------------------
d_list <- read_rds(here(config_obj$paths_config$pitch_sum_path, 
                        "lena-pred-kmeans-outputs.rds")) %>% 
  future_map(~ .x$d_clusters)

### Generate train and test data for the DNN -------------------------------------------------
d_lstm <- future_map(d_list, 
              names(d_list),
              .f = build_lstm_datasets, 
              prop_cds_train_list = config_obj$dnn_dataset_config$prop_train_cds,
              dnn_config = config_obj$dnn_dataset_config)

### Save the output of the generator function -----------------------------------------------
# Note (might have to start splitting data across multiple files for IDSLabel dataset)
write_rds(d_lstm, 
          here(config_obj$paths_config$lstm_sum_path, paste0("lena-pred-lstm-train-test.rds")), 
          compress = "gz")

print("Completed LSTM dataset generation")
