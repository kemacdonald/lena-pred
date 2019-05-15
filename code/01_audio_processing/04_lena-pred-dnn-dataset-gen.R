### Libraries and Helpers -----------------------------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))
source(here("code/00_helper_functions/tests.R"))

### Dataset parameters from config file -----------------------------------------------
source(here("code/00_config/lena-pred-dnn-dataset-config.R"))
source(here("code/00_config/lena-pred-pitch-config.R"))
path <- paste0("data/03_summaries/", dataset_config$data_set)

### Read cluster sequences -----------------------------------------------
d_list <- read_rds(here(path, "lena-pred-kmeans-outputs.rds"))
d_list <- map(d_list, ~ .x$d_clusters)

### Generate train and test data from DNN -------------------------------------------------

# Sample a pool of train and test segments from one of the datasets
d_train_test <- get_train_test(d_list[[1]], dnn_dataset_config$prop_train, dnn_dataset_config$prop_train_cds)

# Pass those train/test samples to the dataset generator function 
# And create lstm-ready datasets for each dataset with different  numbers 
# of q-shapes (e.g., 8, 12, 16, etc.)

d_lstm <- d_list %>%
  map(generate_lstm_dataset, 
      max_seq_len = dnn_dataset_config$seq_max_len,
      skip = dnn_dataset_config$skip_val,
      train_test_split = dnn_dataset_config$prop_train,
      prop_cds = dnn_dataset_config$prop_train_cds)

### Test the generator functions ---------------------------------------------------

# do we have 50/50 ADS/CDS in seg ids of test data
d_train_test$d_test %>% 
  distinct(seg_id, speech_register) %>% 
  count(speech_register) 

# do we have the same duration of CDS and IDS in the training data
d_train_test$d_train %>% 
  distinct(seg_id, speech_register, duration_ms) %>% 
  group_by(speech_register) %>% 
  summarise(total = sum(duration_ms))

### Save the generator function ---------------------------------------------------
write_rds(d_lstm, here(path, "lena-pred-lstm-train-test.rds"), compress = "gz")
