### Train LSTM sequence prediction models ------------------------------

# Neural language models are designed to learn how to predict the next 
# word in a sequence given the prior context. In our case, words are 
# cluster assignments for each 100-ms segment of pitch contour.

# This implementation of the LSTM is borrowed from this tutorial: 
# https://keras.rstudio.com/articles/examples/lstm_text_generation.html

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/plot_helpers.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))
source(here("code/00_config/lena-pred-dnn-config.R"))

# extract vectorized input data for each num q-shapes
inputs <- lstm_config$d

# create list of models for each dataset
mods <- lstm_config$d %>% map(create_lstm, lstm_config)

# train model and generate predictions 
# the train lstm function also does some post-processing 
# of the model predictions
results_obj <- pmap(list(mods, names(mods), inputs), 
                    .f = train_lstm, 
                    lstm_config)

# save predictions for later analysis
write_rds(results_obj, 
          here(read_path, "lena-pred-lstm-preds.rds"), 
          compress = "gz")  