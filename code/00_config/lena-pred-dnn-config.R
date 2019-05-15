#### LSTM Config ----------------------------------------
# These are config parameters for the LSTM 
source(here("code/00_config/lena-pred-pitch-config.R"))
use_rasanen <- FALSE

# set up paths to training sequences for original and new pipepline
read_path <- paste0("data/03_summaries/", dataset_config$data_set)
read_path_rasanen <- "data/03_summaries/rasanen_2018"
rasanen_results <- "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"

# set up config object
lstm_config <- list(
  use_rasanen = use_rasanen,
  lstm_units = 30,
  lstm_output_dim = 30,
  n_epochs = 150,
  early_stop = callback_early_stopping(monitor = "val_loss", 
                                       min_delta = 0.0001, patience = 3, 
                                       verbose = 0, mode = "auto"),
  batch_size = 10,
  epochs = 60,
  validation_split = 0.15, # ask Okko about validation split
  shuffle = TRUE
)

# add config parameters that vary depending on if we use Okko's implementation
if (use_rasanen) {
  lstm_config$input_length <- 10
  lstm_config$n_clusters <- 24
  lstm_config$input_shape <- n_clusters + 1
  lstm_config$d_meta <- readMat(here(read_path_rasanen, rasanen_results))
  lstm_config$d_preds <- readMat(here(read_path_rasanen, "preds_out.mat"))
  lstm_config$d <-readMat(here(read_path_rasanen, "lstm_traindata.mat"))  
} else {
  lstm_config$d <- read_rds(here(read_path, "lena-pred-lstm-train-test.rds"))
  lstm_config$n_clusters <- poly_fit_config$n_q_shapes
  lstm_config$input_shape <- poly_fit_config$n_q_shapes + 1
}