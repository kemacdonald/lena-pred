## LSTM dataset generation helpers ---------------------------------------------------

# wrapper function to map the dataset generator over a list of proportion CDS values
# this allows us to experiment with the amount of CDS in the training data 
build_lstm_datasets <- function(d, d_name, prop_cds_train_list, dnn_config) {
  # sample one set of test data to be used across all the different training sets
  d_test <- get_test_data(d, dnn_config)

  # # sample training data for each proportion of CDS
  d_out <- prop_cds_train_list %>%
    furrr::future_map(.f = build_one_lstm_dataset,
      d = d,
      d_test = d_test,
      dnn_config = dnn_config)

  flatten(d_out)
}

# dataset sampling function based on prop of CDS in training data and prop of data for train/test
# maps over a list of dataframes where each dataframe has cluster sequences with 
# a different number of q-shapes
# returns a list of relevant objects: vectorized data for lstm, some tests of sampling functions

build_one_lstm_dataset <- function(d, prop_cds_train, d_test, dnn_config) {
  # Sample a pool of training segments from one of the datasets returns data frame
  d_train <- get_train_data(d, 
                            test_seg_list = unique(d_test$seg_id),
                            prop_cds = prop_cds_train)
  
  # Create some test objects so we can check that the sampling function works properly
  dur_cds_ads <- check_prop_cds_train(d_train)
  prop_cds_test <- check_prop_cds_test(d_test)
  
  print(dur_cds_ads)

  # Pass those train/test samples to the dataset generator function
  # to create an lstm-ready dataset
  d_out <- generate_lstm_data(d,
                              d_train = d_train,
                              d_test = d_test,
                              max_seq_len = dnn_config$seq_max_len,
                              skip = dnn_config$skip_val)

  d_out$actual_prop_cds_train <- dur_cds_ads
  d_out$actual_prop_cds_test <- prop_cds_test
  d_out$n_qshapes <- unique(d$n_qshapes)

  # return a named list object to track information about the dataset
  obj <- list()
  obj_name <- paste0("cds", prop_cds_train, "_shapes", d_out$n_qshapes)
  obj[[obj_name]] <- d_out

  obj
}

# converts a dataframe with cluster sequences to a list of vectorized train/test
# sequences ready to be fed to the LSTM
generate_lstm_data <- function(d, d_train, d_test, max_seq_len, skip) {
  # extract the cluster sequence as character string
  cluster_sequence <- d %>% pull(cluster)
  
  # get the unique clusters
  unique_clusters <- cluster_sequence %>% unique() %>% sort()
  
  # create a matrix of one-hot vectors encoding
  cluster_dict <- (unique_clusters - 1) %>% keras::to_categorical(num_classes = length(unique_clusters))
  
  # get the training and test cluster sequences 
  d_train <- d_train %>% left_join(d, by = c("seg_id", "time_bin_id", "speech_register", "duration_ms"))
  d_test <- d_test %>% left_join(d, by = c("seg_id", "time_bin_id", "speech_register", "duration_ms"))
  
  # create the sub-sequences based on max_seq_len and skip parameters
  d_out <- list(train_data = make_lstm_sub_sequences(d_train, max_seq_len, skip),
                test_data = make_lstm_sub_sequences(d_test, max_seq_len, skip))
  
  # add one_hot encoding for next cluster (y) in training and test datasets
  d_out$train_data$next_cluster_one_hot <- one_hot_encode(d_out$train_data$next_cluster, cluster_dict)
  d_out$test_data$next_cluster_one_hot <- one_hot_encode(d_out$test_data$next_cluster, cluster_dict)
  
  # vectorize data for passing to LSTM
  d_out$d_vectorized <- list(
    train_in = vectorize_data(d_out$train_data, data_type = "input"),
    train_out = vectorize_data(d_out$train_data, data_type = "output"),
    test_in = vectorize_data(d_out$test_data, data_type = "input"),
    test_out = vectorize_data(d_out$test_data, data_type = "output"))
  
  d_out
}

# create a list of lists that stores all information for training and test
# each element is a question/answer pairs for the model to learn
# questions are the previous n pitch shapes
# answers are th next pitch shape in the sequence
# we also store some metadata about the utterance: id, time_bin_id (position in utt), 
# and speech register
make_lstm_sub_sequences <- function(d_input, max_seq_len, skip) {
  
  data_seq <- seq(1, length(d_input$cluster) - max_seq_len - 1, by = skip)
  
  data_seq %>%
    furrr::future_map(~ list(
      prev_cluster_seq = d_input$cluster[.x:(.x + max_seq_len - 1)],
      next_cluster = d_input$cluster[.x + max_seq_len],
      next_cluster_seg_id = d_input$seg_id[.x + max_seq_len],
      next_cluster_time_bin_id = d_input$time_bin_id[.x + max_seq_len],
      next_cluster_speech_register = d_input$speech_register[.x + max_seq_len],
      next_cluster_speaker_id = d_input$speaker_id[.x + max_seq_len],
      next_cluster_dataset = d_input$dataset[.x + max_seq_len],
      next_cluster_duration_ms = d_input$duration_ms[.x + max_seq_len]
    )) %>%
    transpose()
}

one_hot_encode <- function(cluster_list, cluster_dict) {
  cluster_list %>% map(~ cluster_dict[.x, ]) 
}

get_test_data <- function(d, dnn_config) {
  # get the number of segements to sample from each speaker for ADS and IDS
  d_seg_ids <- d %>% distinct(seg_id, speech_register) 
  n_total <- nrow(d_seg_ids)
  n_speakers <- d %>% distinct(speaker_id) %>% nrow()
  # get the number of training vs. test segments 
  n_to_sample_train <- as.integer(n_total * dnn_config$prop_train)
  n_to_sample_test <- n_total - n_to_sample_train
  
  # get the number of segs to sample from each  speaker
  n_per_speech_reg_test <- ceiling( (n_to_sample_test * dnn_config$prop_test_cds) / n_speakers )
  
  # get seg ids for test data and add a sample id to keep track 
  test_seg_ids <- d %>%
    distinct(seg_id, speech_register, speaker_id) %>%
    group_by(speech_register, speaker_id) %>%
    sample_n(n_per_speech_reg_test, replace = FALSE) %>%
    ungroup() %>%
    select(seg_id) %>%
    mutate(sample_id = 1:n())

  d %>%
    filter(seg_id %in% test_seg_ids$seg_id) %>%
    select(seg_id, speech_register, time_bin_id, duration_ms) %>%
    left_join(., test_seg_ids, by = c("seg_id"))
}

get_train_data <- function(d, test_seg_list, prop_cds) {
  # get train seg ids by removing ids that are already in test pool
  train_segs <- d %>% 
    filter(!(seg_id %in% test_seg_list)) %>% 
    distinct(seg_id, speech_register)
  
  # get the training data pool
  d_train <- d %>% 
    filter(seg_id %in% train_segs$seg_id) %>% 
    distinct(seg_id, speech_register) %>% 
    mutate(sample_id = 1:n()) %>% 
    left_join(., d)
  
  # get the id list for both speech registers
  train_seg_id_list <- d_train %>% distinct(seg_id) %>% pull()
  
  # get the duration of IDS in the train  pool. this will be target total duration
  target_duration_total <- get_reg_duration(d_train, "IDS")
  target_duration_ids <- target_duration_total * prop_cds
  target_duration_ads <- target_duration_total - target_duration_ids
  
  # sample until we get the correct durations for ids and ads
  d_train_ids <- sample_matched_duration(target_duration_ids, d_train, register = "IDS")
  d_train_ads <- sample_matched_duration(target_duration_ads, d_train, register = "ADS")
  
  # bind the ADS and IDS samples together
  bind_rows(d_train_ids, d_train_ads) %>% 
    select(seg_id, speech_register, time_bin_id, duration_ms, sample_id)
}

sample_matched_duration <- function(target_duration, d_train, register) {
  duration <- 0
  samp_id <- max(d_train$sample_id) + 1
  d_out <- data.frame()
  
  seg_id_list <- d_train %>% filter(speech_register == register) %>% distinct(seg_id) %>% pull()
  
  while( duration < target_duration ) {
    seg_id_curr <- sample(seg_id_list, size = 1, replace = FALSE)
    d_curr <- d_train %>% filter(seg_id == seg_id_curr) %>% mutate(sample_id = samp_id)
    d_out <- bind_rows(d_out, d_curr)
    duration <- get_reg_duration(d_out, register)
    samp_id <- samp_id + 1
  }
  d_out
}

# check proportion of CDS/IDS duration in the training data
check_prop_cds_train <- function(d) {
  d %>% 
    distinct(seg_id, speech_register, duration_ms, sample_id) %>% 
    group_by(speech_register) %>% 
    summarise(duration = sum(duration_ms)) %>% 
    mutate(prop = duration / sum(duration))
}

# check proportion of CDS/IDS utterances in the test data
check_prop_cds_test <- function(d) {
  d %>% 
    distinct(seg_id, speech_register, sample_id) %>% 
    count(speech_register, name = "n_utts") 
}

# get duration of utterances  
get_reg_duration <- function(d, register) {
  d %>% 
    filter(speech_register == register) %>% 
    distinct(sample_id, duration_ms) %>% 
    pull(duration_ms) %>% 
    sum()
}

vectorize_data <- function(data_list, data_type) {
  if (data_type == "input") {
    data_list$prev_cluster_seq %>% 
      unlist() %>% 
      matrix(byrow = TRUE, nrow = length(data_list$next_cluster))  
  } else if (data_type == "output") {
    data_list$next_cluster_one_hot %>% 
      unlist() %>% 
      matrix(byrow=TRUE, nrow=length(data_list$next_cluster_one_hot)) 
  } else {
    message("please specify valid data type")
  }
  
} 

# clean up names of predictions
fix_names <- function(x) gsub("V", "class_", x)

# upsample_train_data <- function(seg_id_in, df, sample_id) {
#   df %>% filter(seg_id == seg_id_in) %>% mutate(train_sample_id = sample_id)
# }
