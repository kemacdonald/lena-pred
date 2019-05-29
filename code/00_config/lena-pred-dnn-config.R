#### LSTM Config ----------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-pitch-config.R"))

# set up config object
lstm_config <- list(
  lstm_units = 30,
  lstm_output_dim = 30,
  n_epochs = 12,
  early_stop = callback_early_stopping(monitor = "val_loss", 
                                       min_delta = 0.0001, patience = 3, 
                                       verbose = 0, mode = "auto"),
  batch_size = 10,
  validation_split = 0.1, # ask Okko about validation split
  dropout = 0,
  lr = .001,
  shuffle = TRUE
)

# add config parameters that vary depending on if we use Okko's implementation
if ( paths_config$data_set == "rasanen_2018" ) {
  read_path_rasanen <- "data/03_summaries/rasanen_2018"
  rasanen_results <- "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"
  lstm_config$input_length <- 10
  lstm_config$n_clusters <- 24
  lstm_config$input_shape <- n_clusters + 1
  lstm_config$d_meta <- readMat(here(read_path_rasanen, rasanen_results))
  lstm_config$d_preds <- readMat(here(read_path_rasanen, "preds_out.mat"))
  lstm_config$d <-readMat(here(read_path_rasanen, "lstm_traindata.mat"))  
} else {
  lstm_config$n_clusters <- poly_fit_config$n_q_shapes
  lstm_config$input_shape <- poly_fit_config$n_q_shapes + 1
}
