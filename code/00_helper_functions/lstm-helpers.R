######## LSTM helpers

generate_lstm_dataset <- function(d, 
                                  max_seq_len, 
                                  skip, 
                                  train_test_split = 0.9,
                                  prop_cds = 0.5) {
  
  # extract the cluster sequence as character string
  cluster_sequence <- d %>% pull(cluster)
  
  # get the unique clusters 
  unique_clusters <- cluster_sequence %>% unique() %>% sort()
  
  # create a matrix of one-hot vectors encoding
  cluster_dict <- unique_clusters %>% keras::to_categorical()
  
  # get the training and test cluster sequences using
  seg_ids_to_use <- get_sample_seg_ids(d, train_test_split, prop_cds)
  d_train <- d %>% filter(seg_id %in% seg_ids_to_use$train_seg_ids)
  d_test <- d %>% filter(seg_id %in% seg_ids_to_use$test_seg_ids) 
  
  # create the sub-sequences based on max_seq_len and skip parameters
  d_out <- list(train_data = make_lstm_sub_sequences(d_train, max_seq_len, skip),
                test_data = make_lstm_sub_sequences(d_test, max_seq_len, skip))
  
  # add one_hot encoding for next cluster (y) in training and test datasets
  d_out$train_data$next_cluster_one_hot <- one_hot_encode(d_out$train_data$next_cluster, cluster_dict)
  d_out$test_data$next_cluster_one_hot <- one_hot_encode(d_out$test_data$next_cluster, cluster_dict)
  
  d_out
}

get_sample_seg_ids <- function(d, train_test_split, prop_cds) {
  d %>% distinct(seg_id, speech_register) -> d
  
  n_to_sample <- as.integer(nrow(d) * train_test_split)
  n_per_group <- as.integer(n_to_sample * prop_cds)
  
  train_seg_ids <- d %>% 
    group_by(speech_register) %>% 
    sample_n(n_per_group) %>% 
    pull(seg_id)
  
  test_seg_ids <- setdiff(unique(d$seg_id), train_seg_ids)
  
  list(train_seg_ids = train_seg_ids, test_seg_ids = test_seg_ids)
}


# create a list of lists that stores all information for training and test
# each element is a question/answer pairs for the model to learn
# questions are the previous n pitch shapes
# answers are th next pitch shape in the sequence
# we also store some metadata about the utterance: id, time_bin_id (position in utt), and speech register
make_lstm_sub_sequences <- function(d_input, max_seq_len, skip) {
  
  data_seq <- seq(1, length(d_input$cluster) - max_seq_len - 1, by = skip)
  
  data_seq %>%
    map(~  list(
      prev_cluster_seq = d_input$cluster[.x:(.x + max_seq_len - 1)],
      next_cluster = d_input$cluster[.x + max_seq_len],
      next_cluster_seg_id = d_input$seg_id[.x + max_seq_len],
      next_cluster_time_bin_id = d_input$time_bin_id[.x + max_seq_len],
      next_cluster_speech_register = d_input$speech_register[.x + max_seq_len],
      next_cluster_dataset = d_input$dataset[.x + max_seq_len])
    ) %>%
    transpose()
}

one_hot_encode <- function(cluster_list, cluster_dict) {
  cluster_list %>% map(~ cluster_dict[.x, ]) 
}

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
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