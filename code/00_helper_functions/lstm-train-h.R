
# LSTM training helpers ---------------------------------------------------

run_k_fold <- function(d, k = 10, dnn_config) {
  # extract info needed to specify features of test dataset
  all_seg_ids <- d %>% distinct(seg_id, speech_register, speaker_id)
  n_test <- as.integer(nrow(all_seg_ids) * (1 - dnn_config$prop_train))
  n_per_register_test <- n_test / 2
  n_speakers <- d %>% distinct(speaker_id) %>% nrow()
  n_per_speaker_test <- as.integer(n_per_register_test / n_speakers)
  segs_per_speaker <- all_seg_ids %>% count(speaker_id, speech_register)
  speaker_blacklist <- segs_per_speaker %>% filter(n < n_per_speaker_test) %>% pull(speaker_id)
  
  1:k %>% map(create_fold, 
              d = d,
              seg_info = all_seg_ids, 
              blacklist = speaker_blacklist, 
              size_per_speaker = n_per_speaker_test)
}

create_fold <- function(fold, d, seg_info, blacklist, size_per_speaker) {
  test_seg_ids <- seg_info %>% 
    filter( !(speaker_id %in% blacklist) ) %>% 
    group_by(speech_register, speaker_id) %>% 
    sample_n(size = size_per_speaker, replace = FALSE) %>% 
    ungroup() %>%
    select(speaker_id, seg_id) %>%
    mutate(sample_id = 1:n(),
           fold_id = fold)
  
  d %>%
    filter(seg_id %in% test_seg_ids$seg_id) %>%
    select(speaker_id, seg_id, speech_register, time_bin_id, duration_ms) %>%
    left_join(., test_seg_ids, by = c("seg_id", "speaker_id"))
}

create_lstm <- function(d, lstm_config) {
  # extract some model params for tensor shaping
  n_shapes <- d$n_qshapes
  input_shape <- n_shapes + 1
  input_length <- d$train_data$prev_cluster_seq[[1]] %>% length()
  
  model <- keras_model_sequential()
  
  model %>%
    layer_embedding(
      input_dim = input_shape,
      output_dim = lstm_config$lstm_output_dim,
      input_length = input_length) %>%
    layer_lstm(units = lstm_config$lstm_units,
               input_shape = c(input_length, input_shape),
               dropout = lstm_config$dropout,
               return_sequences = F) %>%
    layer_dense(n_shapes, activation = "softmax")
  
  ## multi-layer lstm
  # model %>%
  #   layer_embedding(
  #     input_dim = input_shape,
  #     output_dim = lstm_config$lstm_output_dim,
  #     input_length = input_length) %>%
  #   layer_lstm(units = lstm_config$lstm_units,
  #              input_shape = c(input_length, input_shape),
  #              dropout = lstm_config$dropout,
  #              return_sequences = TRUE) %>%
  #   layer_lstm(units = lstm_config$lstm_units,
  #              input_shape = c(input_length, input_shape),
  #              dropout = lstm_config$dropout,
  #              return_sequences = FALSE) %>%
  #   layer_dense(n_shapes, activation = "softmax")
  
  optimizer <- optimizer_rmsprop(lr = lstm_config$lr)
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer,
    metrics = list('accuracy')
  )
  
  model
}

# Train LSTM ---------------------------------------------------

train_lstm <- function(model, model_name, input_data, lstm_config) {
  summary(model)
  
  # train model
  if(lstm_config$include_early_stop) {
    m_fit <- model %>%
      fit(input_data$d_vectorized$train_in,
          input_data$d_vectorized$train_out,
          batch_size = lstm_config$batch_size,
          epochs = lstm_config$n_epochs,
          validation_split = lstm_config$validation_split,
          shuffle = lstm_config$shuffle,
          callbacks = lstm_config$early_stop
      )  
  } else {
    m_fit <- model %>%
      fit(input_data$d_vectorized$train_in,
          input_data$d_vectorized$train_out,
          batch_size = lstm_config$batch_size,
          epochs = lstm_config$n_epochs,
          validation_split = lstm_config$validation_split,
          shuffle = lstm_config$shuffle
      )  
  }
  
  # generate predictions
  preds <- model %>% predict(input_data$d_vectorized$test_in)
  
  # tidy up predictions
  d_tidy_preds <- tidy_preds(preds, input_data, input_data$exp_run_id)
  
  # save model
  if (lstm_config$save_model) {model %>% save_model_hdf5(here(glue::glue("models/", {model_name}, "_mod.h5")))}
  
  list(fit = m_fit, d_preds = d_tidy_preds)
}

# Safe train lstm ---------------------------------------------------------

safe_train_lstm <- safely(train_lstm)

# Tidy model predictions --------------------------------------------------

tidy_preds <- function(preds, d, run_id, nqshapes) {
  
  d_preds <- preds %>% 
    as_tibble(.name_repair = "universal") %>% 
    clean_names()
  
  colnames(d_preds) <- colnames(d_preds) %>% str_replace("x", "shape_")
  
  d_preds <- d_preds %>% 
    mutate(seg_id = d$test_data$next_cluster_seg_id %>% as.character(),
           speaker_id = d$test_data$next_cluster_speaker_id %>% as.character(),
           exp_run_id = run_id,
           speech_register = d$test_data$next_cluster_speech_register %>% as.character(),
           time_bin_id = d$test_data$next_cluster_time_bin_id %>% as.character(),
           target_cluster = d$test_data$next_cluster %>% as.character(),
           dataset = d$test_data$next_cluster_dataset %>% as.character(),
           duration_ms = d$test_data$next_cluster_duration_ms %>% as.character()) %>% 
    select(seg_id, speaker_id, exp_run_id, dataset, speech_register, 
           time_bin_id, target_cluster, duration_ms, everything()) 
  
  # tidy the predictions
  first_shape_col <- "shape_1"
  last_shape_col <- colnames(d_preds)[colnames(d_preds) %>% length()]
  
  d_preds_tidy <- d_preds %>% 
    gather(key = "cluster_shape", 
           value = "prob_mass", 
           first_shape_col:last_shape_col)
  
  d_preds_tidy %>% 
    group_by(seg_id, time_bin_id) %>% 
    mutate(is_target_cluster = ifelse(str_extract(cluster_shape, "\\d+") == target_cluster, TRUE, FALSE),
           predicted_cluster = which.max(prob_mass),
           dataset = str_to_lower(dataset),
           correct_pred = ifelse(target_cluster == predicted_cluster, 1, 0)) 
}