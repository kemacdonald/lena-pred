######## LSTM helpers

generate_lstm_dataset <- function(input_seq, max_seq_len, skip, class_matrix) {
  data_seq <- seq(1, length(cluster_sequence) - max_seq_len - 1, by = skip)
  
  data_seq %>% 
    map(~list(prev_cluster_seq = cluster_sequence[.x:(.x + max_seq_len - 1)], 
              next_cluster = cluster_sequence[.x + max_seq_len])) %>% 
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