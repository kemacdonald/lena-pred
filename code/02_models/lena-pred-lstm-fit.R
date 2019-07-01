# Train LSTM sequence prediction models ------------------------------
# Neural language models are designed to learn how to predict the next 
# word in a sequence given the prior context. In our case, words are 
# cluster assignments for each 100-ms segment of pitch contour.
# This implementation of the LSTM is adapted from this tutorial: 
# https://keras.rstudio.com/articles/examples/lstm_text_generation.html

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/lstm-train-h.R"))
source(here("code/00_config/lena-pred-config.R"))

# get data and flatten to a one level list of datasets to fit
# should be n_prop_cds X n_q_shapes datasets in the list
d_list <- read_rds(here(config_obj$paths_config$lstm_sum_path, 
                        "lena-pred-lstm-train-test.rds")) %>% 
  flatten() 

# create list of models for each dataset: prop CDS and n-qshapes
mods <- d_list %>% map(create_lstm, config_obj$lstm_config)

# train model and generate predictions 
# the train lstm function also handles post-processing 
# and tidying the model predictions
results_obj <- pmap(list(mods, names(mods), d_list), 
                    .f = safe_train_lstm, 
                    save_model = FALSE,
                    lstm_config = config_obj$lstm_config)

# save predictions for later analysis
write_rds(results_obj, 
          here(config_obj$paths_config$lstm_sum_path, "lena-pred-lstm-preds.rds"), 
          compress = "gz")  
