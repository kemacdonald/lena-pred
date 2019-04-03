######## LSTM helpers

generate_lstm_dataset <- function(input_seq, max_seq_len, skip, train_split, cluster_dict) {
  split_idx <- as.integer(length(input_seq) * train_split)
  train_seq <- input_seq[1:split_idx]
  test_seq <- input_seq[(split_idx + 1):length(input_seq)]
  
  length(train_seq) + length(test_seq)
  
  d <- list(train_data = make_lstm_sub_sequences(train_seq, max_seq_len, skip),
            test_data = make_lstm_sub_sequences(test_seq, max_seq_len, skip))
  
  # add one_hot encoding for next cluster output (y)
  d$train_data$next_cluster_one_hot <- one_hot_encode(d_lstm$train_data$next_cluster, cluster_dict)
  d$test_data$next_cluster_one_hot <- one_hot_encode(d_lstm$test_data$next_cluster, cluster_dict)
  
  d

}

make_lstm_sub_sequences <- function(input_seq, max_seq_len, skip) {
  data_seq <- seq(1, length(input_seq) - max_seq_len - 1, by = skip)
  
  data_seq %>% 
    map(~list(prev_cluster_seq = input_seq[.x:(.x + max_seq_len - 1)], 
              next_cluster = input_seq[.x + max_seq_len])) %>% 
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

on_epoch_end <- function(epoch, logs) {
  
  cat(sprintf("epoch: %02d ---------------\n\n", epoch))
  
  for(diversity in c(0.2, 0.5, 1, 1.2)){
    
    cat(sprintf("diversity: %f ---------------\n\n", diversity))
    
    start_index <- sample(1:(length(text) - maxlen), size = 1) # randomly sample a charcter for start index
    sentence <- text[start_index:(start_index + maxlen - 1)]
    generated <- ""
    
    for(i in 1:400){
      
      x <- sapply(chars, function(x){
        as.integer(x == sentence)
      })
      x <- array_reshape(x, c(1, dim(x)))
      
      preds <- predict(model, x)
      next_index <- sample_mod(preds, diversity)
      next_char <- chars[next_index]
      
      generated <- str_c(generated, next_char, collapse = "")
      sentence <- c(sentence[-1], next_char)
      
    }
    
    cat(generated)
    cat("\n\n")
    
  }
}

print_callback <- callback_lambda(on_epoch_end = on_epoch_end)


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