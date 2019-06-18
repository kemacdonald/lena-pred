### Pitch Extraction Config File #### ----------------------------------------------------
set.seed(12345)

### Experiment config ----------------------------------------------------
dataset_name <- "ManyBabies"
prop_cds_vals <- c(0, 0.25, 0.5, 0.75, 1)
n_qshapes_vals <- 12
#n_qshapes_vals <- seq(8, 24, by = 4)

### DNN dataset config ----------------------------------------------------

dnn_dataset_config <- list(
  seq_max_len = 10,     
  skip_val = 1,         
  prop_train = 0.9,
  prop_train_cds = prop_cds_vals,
  prop_test_cds = 0.5
)

### Pitch extraction config ----------------------------------------------------

pitch_detect_config <- list(
  pitch_min = 75, 
  pitch_max = 650,
  silence_min = 0.001,
  step_size =  10,
  window_length = 50,
  window_type = "hanning",
  ent_threshold = 0.6,
  autocor_threshold = 0.7,
  pathfinding_alg = "fast",
  pitch_methods = c("autocor", "spec", "dom"))

time_filter_config <- list(
  min_prop_voiced = 0.1,
  time_bin_width = 100,
  min_samples_bin = 10)

kmeans_config <- list(
  iter_max = 20, 
  scale_coefs = FALSE  
)

loess_config <- list(
  min_n_samples_loess = 10,
  frac_points_loess = 0.1,
  preds_sample_rate = 10)

poly_fit_config <- list(
  degree_poly = 2, 
  n_q_shapes = n_qshapes_vals
)

### Paths config ----------------------------------------------------

path_to_wav <- case_when(
  dataset_name == "pilot" ~ "data/02_processed_data/pilot-segments-norm",
  dataset_name == "ManyBabies" ~ "data/02_processed_data/ManyBabies-norm",
  dataset_name == "IDSLabel" ~ "data/02_processed_data/IDSLabel-norm",
)

# get the audio filenames
files_to_analyze <- list.files(here::here(path_to_wav), 
                               pattern = "*.wav", 
                               recursive = T)

# add the full path
files_to_analyze <- here::here(path_to_wav, files_to_analyze)

paths_config <- list(data_set = dataset_name, 
                     path_to_wav = path_to_wav, 
                     files_to_analyze = files_to_analyze,
                     pitch_sum_path = paste0("data/03_summaries/", dataset_name, "/01_pitch-data"),
                     lstm_sum_path = paste0("data/03_summaries/", dataset_name, "/02_lstm-data"))
