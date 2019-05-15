### LSTM helpers ---------------------------------------------------

generate_lstm_dataset <- function(d, segs_train_test,
                                  max_seq_len, skip, 
                                  train_test_split = 0.9,
                                  prop_cds = 0.5) {
  
  # extract the cluster sequence as character string
  cluster_sequence <- d %>% pull(cluster)
  
  # get the unique clusters
  unique_clusters <- cluster_sequence %>% unique() %>% sort()
  
  # create a matrix of one-hot vectors encoding
  cluster_dict <- (unique_clusters - 1) %>% keras::to_categorical(num_classes = length(unique_clusters))
  
  # get the training and test cluster sequences sampling with prop CDS/ADS in config file
  #d_train_test <- get_train_test(d, train_test_split, prop_cds)
  d_train <- d_train_test$d_train %>% left_join(d, by = c("seg_id", "time_bin_id", "speech_register", "duration_ms"))
  d_test <- d_train_test$d_test %>% left_join(d, by = c("seg_id", "time_bin_id", "speech_register"))
  
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
    test_out = vectorize_data(d_out$test_data, data_type = "output")
  )
  
  d_out
}

# create a list of lists that stores all information for training and test
# each element is a question/answer pairs for the model to learn
# questions are the previous n pitch shapes
# answers are th next pitch shape in the sequence
# we also store some metadata about the utterance: id, time_bin_id (position in utt), and speech register
make_lstm_sub_sequences <- function(d_input, max_seq_len, skip) {
  
  data_seq <- seq(1, length(d_input$cluster) - max_seq_len - 1, by = skip)
  
  data_seq %>%
    map(~ list(
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

get_train_test <- function(d, train_test_split, prop_cds) {
  d_seg_ids <- d %>% distinct(seg_id, speech_register) 
  n_total <- nrow(d_seg_ids)
  n_to_sample_train <- as.integer(n_total * train_test_split)
  n_per_speech_reg_train <- ceiling(n_to_sample_train * prop_cds)
  n_to_sample_test <- n_total - n_to_sample_train
  n_speakers <- d %>% distinct(speaker_id) %>% nrow()
  n_per_speech_reg_test <- ceiling( (n_to_sample_test * prop_cds) / n_speakers )
  
  # get test data
  test_seg_ids <- d %>%
    select(seg_id, speech_register, speaker_id) %>%
    group_by(speech_register, speaker_id) %>%  
    sample_n(n_per_speech_reg_test, replace = FALSE) %>%
    pull(seg_id)
  
  d_test <- d %>% filter(seg_id %in% test_seg_ids) %>% select(seg_id, speech_register, time_bin_id)
  d_train <- sample_training_data(d, test_seg_ids) %>% select(seg_id, speech_register, time_bin_id, duration_ms)
  
  list(d_train = d_train, d_test = d_test)
}

# get train data
sample_training_data <- function(d, test_seg_list) {
  train_segs <- d %>% 
    filter(!(seg_id %in% test_seg_list)) %>% 
    distinct(seg_id, speech_register)
  
  d_train_final <- d %>% filter(seg_id %in% train_segs$seg_id, speech_register == "IDS")
  d_train_ads <- d %>% filter(seg_id %in% train_segs$seg_id, speech_register == "ADS")
  train_ads_id_list <- train_segs %>% filter(speech_register == "ADS") %>% pull(seg_id)
  
  ids_duration <- get_reg_duration(d_train_final, "IDS")
  ads_duration <- 0
  
  while(ads_duration < ids_duration) {
    ads_id <- sample(train_ads_id_list, size = 1)
    d_train_final <- bind_rows(d_train_final, d_train_ads %>% filter(seg_id == ads_id))
    ads_duration <- get_reg_duration(d_train_final, "ADS")
  }
  d_train_final
}

# get duration of utterances  
get_reg_duration <- function(d, register) {
  d %>% 
    filter(speech_register == register) %>% 
    distinct(seg_id, duration_ms) %>% 
    pull(duration_ms) %>% sum()
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

## model creation, training, and predictions --------------------------------

create_lstm <- function(d, lstm_config) {
  # extract some model params for tensor shaping
  n_shapes <- d$train_data$next_cluster %>% unique() %>% length() 
  unique_clusters <- seq(0, n_shapes - 1, by = 1)
  input_shape <- n_shapes + 1
  input_length <- d$train_data$prev_cluster_seq[[1]] %>% length()
  
  # build model
  model <- keras_model_sequential()
  
  model %>%
    layer_embedding(
      input_dim = input_shape, 
      output_dim = lstm_config$lstm_output_dim, 
      input_length = input_length) %>% 
    layer_lstm(units = lstm_config$lstm_units, 
               input_shape = c(input_length, input_shape),
               return_sequences = FALSE) %>%
    layer_dense(n_shapes, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = 'rmsprop',
    metrics = list('accuracy')
  )
  
  model
}

train_lstm <- function(model, model_name, input_data, lstm_config) {
  summary(model)
  
  # train model
  m_fit <- model %>%
    fit(input_data$d_vectorized$train_in,
        input_data$d_vectorized$train_out,
        batch_size = lstm_config$batch_size,
        epochs = lstm_config$n_epochs,
        validation_split = lstm_config$validation_split,
        shuffle = lstm_config$shuffle,
        callbacks = lstm_config$early_stop
    )
  
  # generate and tidy predictions
  preds <- model %>% predict(input_data$d_vectorized$test_in)
  tidy_preds <- tidy_preds(preds, input_data)
  
  # save traind model
  model %>% save_model_hdf5(
    here(glue::glue("code/02_models/", {model_name}, "_mod.h5"))
  )
  
  list(fit = m_fit, d_preds = tidy_preds)
}

tidy_preds <- function(preds, d) {
  
  d_preds <- preds %>% 
    as_tibble(.name_repair = "universal") %>% 
    clean_names()
  
  colnames(d_preds) <- colnames(d_preds) %>% str_replace("x", "shape_")
  
  d_preds %>% 
    mutate(seg_id = d$test_data$next_cluster_seg_id %>% as.character(),
           speaker_id = d$test_data$next_cluster_speaker_id %>% as.character(),
           speech_register = d$test_data$next_cluster_speech_register %>% as.character(),
           time_bin_id = d$test_data$next_cluster_time_bin_id %>% as.character(),
           target_cluster = d$test_data$next_cluster %>% as.character(),
           dataset = d$test_data$next_cluster_dataset %>% as.character(),
           duration_ms = d$test_data$next_cluster_duration_ms %>% as.character()) %>% 
    select(seg_id, speaker_id, dataset, speech_register, time_bin_id, target_cluster, duration_ms, everything()) -> d_preds
  
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

# extract just tidy preds data frame from results object
extract_preds <- function(d_list) {
  map2_dfr(d_list, names(d_list), function(x, y) x[[2]] %>% 
             mutate(n_qshapes = str_extract(y, pattern = "[:digit:]+") %>% as.numeric()))
}

# upsample_train_data <- function(seg_id_in, df, sample_id) {
#   df %>% filter(seg_id == seg_id_in) %>% mutate(train_sample_id = sample_id)
# }

# TODO: fix this function
compute_perplexity <- function(d) {
  cross_entropy <- keras::loss_categorical_crossentropy(y_true, y_pred)
  2^cross_entropy
}

# TODO: fix this function  to handle duration model
get_bda_results <- function(m_fit) {
  post <- posterior_samples(m_fit)
  
  m_summary <- post %>% 
    mutate(p_ADS = inv_logit_scaled(b_Intercept),
           p_IDS = inv_logit_scaled(b_Intercept + b_speech_registerIDS),
           diff_correct = p_ADS - p_IDS,
           sample_id = 1:nrow(.)) %>% 
    select(sample_id, p_ADS, p_IDS, diff_correct) %>% 
    gather(key = "type", value = "value", -sample_id) %>% 
    group_by(type) %>% 
    median_hdi(.width = 0.95)
  
  model_results <- list(post_samples = post, summary = m_summary)
}
