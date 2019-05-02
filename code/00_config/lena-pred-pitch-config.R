### Pitch Extraction Config File ####

set.seed(12345)

pitch_detect_config <- list(
  pitch_min = 75, 
  pitch_max = 650,
  silence_min = 0.02,
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

loess_config <- list(
  min_n_samples_loess = 20,
  frac_points_loess = 0.2,
  preds_sample_rate = 10)

poly_fit_config <- list(
  degree_poly = 2, 
  n_q_shapes = 20 )

dnn_dataset_config <- list(
  seq_max_len = 10,     
  skip_val = 1,         
  prop_train = 0.9,     
  prop_train_cds = 0.5)


# path to audio
path_to_wav <- "data/02_processed_data/pilot-segments-norm"

files_to_analyze <- list.files(here::here(path_to_wav), 
                               pattern = "*.wav", 
                               recursive = T)

# add the full path
files_to_analyze <- here::here(path_to_wav, files_to_analyze)