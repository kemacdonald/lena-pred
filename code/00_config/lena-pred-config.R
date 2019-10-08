# Pitch Config File -------------------------------------------------------
set.seed(1234567)
datasets <- list(pilot = "pilot", mb = "ManyBabies", lena = "IDSLabel")
dataset_name <- datasets[['lena']]
debug_exp <- FALSE

if (debug_exp) {
  runs <- c("run1", "run2") 
  n_folds <- 2
  prop_cds_vals <-  c(0.5, 1)
  n_qshapes_vals <- c(12)   
  n_epochs <- 1
  batch_size <- 100
} else {
  runs <- c("run1", "run2", "run3", "run4", "run5")
  n_folds <- 5
  prop_cds_vals <-  c(0, 0.25, 0.5, 0.75, 1)
  n_qshapes_vals <- c(6, 12, 24)   
  n_epochs <- 15
  batch_size <- 50
}

path_to_wav <- case_when(
  dataset_name == "pilot" ~ "data/02_processed_data/pilot-segments-norm",
  dataset_name == "ManyBabies" ~ "data/02_processed_data/ManyBabies-norm",
  dataset_name == "IDSLabel" ~ "data/02_processed_data/IDSLabel-norm",
)

files_to_analyze <- list.files(here::here(path_to_wav), pattern = "*.wav", recursive = T)
files_to_analyze <- here::here(path_to_wav, files_to_analyze)

# Config Object -------------------------------------------------------

config_obj <- list(
  exp_config = list(
    dataset_name = dataset_name,
    prop_cds_vals = prop_cds_vals,
    n_qshapes_vals = n_qshapes_vals,
    n_folds = n_folds,
    runs = runs
  ),
  
  dnn_dataset_config = list(
    seq_max_len = 10,     
    skip_val = 1,         
    prop_train = 0.9,
    prop_train_cds = prop_cds_vals,
    prop_test_cds = 0.5
  ), 
  
  pitch_detect_config = list(
    pitch_min = 75, 
    pitch_max = 650,
    silence_min = 0.001,
    step_size =  10,
    window_length = 50,
    window_type = "hanning",
    ent_threshold = 0.6,
    autocor_threshold = 0.7,
    pathfinding_alg = "fast",
    pitch_methods = c("autocor", "spec", "dom")
  ),
  
  time_filter_config = list(
    min_prop_voiced = 0.1,
    time_bin_width = 100,
    min_samples_bin = 10
  ),
  
  kmeans_config = list(
    iter_max = 20, 
    scale_coefs = "scaled"  
  ),
  
  loess_config = list(
    min_n_samples_loess = 10,
    frac_points_loess = 0.2,
    preds_sample_rate = 10),
  
  poly_fit_config = list(
    degree_poly = 2, 
    n_q_shapes = n_qshapes_vals
  ),
  
  paths_config = list(data_set = dataset_name, 
                      path_to_wav = path_to_wav, 
                      files_to_analyze = files_to_analyze,
                      pitch_sum_path = paste0("data/03_summaries/", dataset_name, "/01_pitch-data/"),
                      lstm_sum_path = paste0("data/03_summaries/", dataset_name, "/02_lstm-data/"),
                      lstm_preds_path = paste0("data/03_summaries/", dataset_name, "/02_lstm-data/experimental_runs")),
  
  lstm_config = list(
    lstm_units = 30,
    lstm_output_dim = 30,
    n_epochs = n_epochs,
    include_early_stop = FALSE, # set to FALSE if we want the same number of training epochs across "conditions"
    early_stop = callback_early_stopping(monitor = "val_loss", 
                                         min_delta = 0.0001, patience = 3, 
                                         verbose = 0, mode = "auto"),
    batch_size = batch_size, 
    validation_split = 0, # use all data for training
    dropout = 0,
    lr = .01,
    shuffle = TRUE,
    save_model = FALSE,
    n_clusters = n_qshapes_vals,
    input_shape = n_qshapes_vals + 1
  )
)
