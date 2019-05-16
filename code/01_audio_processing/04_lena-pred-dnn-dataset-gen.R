### Libraries and Helpers -----------------------------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))
source(here("code/00_helper_functions/tests.R"))

### Dataset parameters from config file -----------------------------------------------
source(here("code/00_config/lena-pred-pitch-config.R"))
path <- paste0("data/03_summaries/", dataset_config$data_set)

### Read cluster sequences -----------------------------------------------
d_list <- read_rds(here(path, "lena-pred-kmeans-outputs.rds"))
d_list <- map(d_list, ~ .x$d_clusters)

### Generate train and test data from DNN -------------------------------------------------

# Sample a pool of train and test segments from one of the datasets
train_test_samples <- get_train_test(d_list[[1]], 
                               train_test_split = dnn_dataset_config$prop_train, 
                               prop_cds_train = dnn_dataset_config$prop_train_cds, 
                               prop_cds_test = dnn_dataset_config$prop_test_cds)

### Test the generator functions ---------------------------------------------------

# do we have 50/50 ADS/CDS utterances in test data
train_test_samples$d_test %>% 
  distinct(seg_id, speech_register, sample_id) %>% 
  count(speech_register) 

# what proportion of CDS/IDS duration in the training data
train_test_samples$d_train %>% 
  distinct(seg_id, speech_register, duration_ms, sample_id) %>% 
  group_by(speech_register) %>% 
  summarise(duration = sum(duration_ms)) %>% 
  mutate(prop = duration / sum(duration))

# Pass those train/test samples to the dataset generator function 
# And create lstm-ready datasets for each dataset with different  numbers 
# of q-shapes (e.g., 8, 12, 16, etc.)

d_lstm <- d_list %>% 
  map(generate_lstm_dataset, 
      d_train_test = train_test_samples,
      max_seq_len = dnn_dataset_config$seq_max_len,
      skip = dnn_dataset_config$skip_val)

### Save the output of the generator function -----------------------------------------------
write_rds(d_lstm, here(path, "lena-pred-lstm-train-test.rds"), compress = "gz")
