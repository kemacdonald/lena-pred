##### Pitch processing helper functions 

# function to process and extract pitch contour from .wav file
# returns a data frame with the segment id, dataset, pitch values, and amplitude
get_pitch_contour <- function(file_path, p_config) {
  file_path_spl <- str_split(file_path, "/", simplify = T)
  
  d_out <- analyze(x = file_path,
                   plot = FALSE,
                   pitchFloor = p_config$pitch_min,
                   pitchCeiling = p_config$pitch_max,
                   silence = p_config$silence_min,
                   autocorThres = p_config$autocor_threshold,
                   pathfinding = p_config$pathfinding_alg,
                   pitchMethods = p_config$pitch_methods,
                   entropyThres = p_config$ent_threshold,
                   step = p_config$step_size,
                   wn = p_config$window_type,
                   windowLength = p_config$window_length)
  
  tidy_seg_meta(d_out, file_path, file_path_spl)
}

# takes path to wav and extracts speaker for manybabies audio files
get_speaker_id_mb <- function(s) {str_extract(s, "(?<=_?)\\d+")}

# tidy audio file metadata based on checking the dataset in the file path
# returns a data frame with appropriate metadata extracted from file path
tidy_seg_meta <- function(d, file_path, file_path_spl) {
  
  if ( str_detect(file_path, "pilot") ) {
    d %>%
      mutate(dataset = file_path_spl[10],
             path_to_wav = file_path,
             speech_register = file_path_spl[11],
             word_category = NA,
             seg_id = str_remove(file_path_spl[12], '.wav'),
             speaker_id = NA) %>%
      select(seg_id, dataset, speech_register, speaker_id, word_category, pitch, voiced, time, ampl, path_to_wav)
  } else if ( str_detect(file_path, "ManyBabies") ) {
    d %>%
      mutate(dataset = str_remove(file_path_spl[9], "-norm"),
             path_to_wav = file_path,
             speech_register = file_path_spl[10],
             word_category = file_path_spl[11],
             seg_id = str_remove(file_path_spl[12], '.wav'),
             speaker_id = get_speaker_id_mb(seg_id)) %>% 
      select(seg_id, dataset, speech_register, speaker_id, word_category, pitch, voiced, time, ampl, loudness, path_to_wav)
  } else if ( str_detect(file_path, "IDSLabel") ) {
    # TODO: make this work with whatever metadata we can extract from IDSLabel filenames
    d %>%
      mutate(dataset = file_path_spl[9],
             path_to_wav = file_path,
             speech_register = file_path_spl[10],
             word_category = NA,
             seg_id = str_remove(file_path_spl[11], '.wav'),
             speaker_id = NA) %>%
      select(seg_id, dataset, speech_register, word_category, pitch, voiced, time, ampl, loudness, path_to_wav)
  } else {
    print("invalid specification of dataset in path_to_wav in config file")
  }
}

# filters all segments in a dataframe, removing samples before and after 
# the first pitch candidate
# also re-zeroes the time variable to start at zero
batch_filter_voiced <- function(d) {
  d %>% 
    split(.$seg_id) %>% 
    furrr::future_map_dfr(filter_voiced)
}

filter_voiced <- function(d) {
  min_cut_point <- d %>% filter(!is.na(pitch)) %>% pull(time) %>% min()
  max_cut_point <- d %>% filter(!is.na(pitch)) %>%  pull(time) %>% max()
  
  d %>% 
    filter(time >= min_cut_point & time <= max_cut_point) %>% 
    mutate(time = time - min_cut_point)
}

# use loess model to intrpolate between pitch estimates
batch_interpolate <- function(d, loess_config) {
  d %>% 
    split(.$seg_id) %>% 
    furrr::future_map_dfr(interpolate_loess, 
                          frac_points = loess_config$frac_points_loess, 
                          sample_rate = loess_config$preds_sample_rate) %>% 
    filter(!is.na(log_pitch_interp)) 
}

interpolate_loess <- function(d, sample_rate, frac_points) {
  
  t_to_predict <- d$time
  
  preds <- loess(log_pitch ~ time, 
                 data = d, 
                 span = frac_points) %>% 
    predict(t_to_predict) 
  
  tibble(
    seg_id = d$seg_id[1],
    exp_run_id = d$exp_run_id[1],
    speaker_id = d$speaker_id[1],
    dataset = d$dataset[1],
    speech_register = d$speech_register[1],
    time = t_to_predict, 
    log_pitch_original = d$log_pitch,
    log_pitch_interp = preds,
    pitch_original = d$pitch
  ) 
}

# creates equally-spaced time bins for each clip
create_time_bins <- function(d, bin_width) {
  d %>% 
    group_by(seg_id) %>% 
    mutate(time_bin = cut_width(time, 
                                width = bin_width,
                                closed = 'left',
                                boundary = 0)) 
}

# re-zero time wrt to this 100 ms time bin
get_time_in_bin <- function(d, sample_rate) {
  d %>% 
    group_by(seg_id, time_bin) %>% 
    mutate(time_wrt_bin = seq_along(time_bin) * sample_rate,
           n_bins_in_seg = n())
}

# create more meaningful bin labels for each 100 ms time bin
relabel_bins <- function(d) {
  d %>% 
    distinct(seg_id, time_bin) %>% 
    group_by(seg_id) %>% 
    mutate(time_bin_id = seq_along(time_bin)) %>% 
    left_join(d, ., by = c("seg_id", "time_bin"))
}

map_get_clusters <- function(k_list, run_id, d, scale_coefs = TRUE, iter_max = 20) {
  names(k_list) <- paste0("shapes_", as.character(k_list), "-run", run_id)
  k_list %>% furrr::future_map(.f = get_cluster_assignments, df = d, scale_coefs = scale_coefs, iter_max = iter_max) 
}

# returns kmeans cluster assignments for each 100 ms time bin
get_cluster_assignments <- function(df, k, scale_coefs = TRUE, iter_max = 20) {
  
  if (scale_coefs) {
    m_linear <- df$coef_linear %>% mean()
    m_quad <- df$coef_quadratic %>% mean()
    sd_linear <- df$coef_linear %>% sd()
    sd_quad <- df$coef_quadratic %>% sd()
    
    coefs_mat <- df %>% 
      select(coef_quadratic, coef_linear) %>% 
      as.matrix() %>% 
      scale()
    
    cl <- kmeans(coefs_mat, centers = k, iter.max = iter_max)
    
    list(centers = cl$centers %>% as_tibble(rownames = "cluster") %>% 
           mutate(coef_intercept = 0,  cluster = as.numeric(cluster),
                  coef_linear_unscaled = coef_linear * sd_linear + m_linear,
                  coef_quadratic_unscaled = coef_quadratic * sd_quad + m_quad),
         d_clusters = df %>% mutate(cluster = cl$cluster, n_qshapes = k))
  } else {
    coefs_mat <- df %>% 
      select(coef_quadratic, coef_linear) %>% 
      as.matrix()  
    
    cl <- kmeans(coefs_mat, centers = k, iter.max = iter_max)
    
    list(centers = cl$centers %>% as_tibble(rownames = "cluster") %>% 
           mutate(coef_intercept = 0,  cluster = as.numeric(cluster)),
         d_clusters = df %>% mutate(cluster = cl$cluster, n_qshapes = k))
  }
}

# add the duration of utterances to the dataframe
add_utt_duration_segs <- function(d) {
  d %>% 
    group_by(seg_id) %>% 
    mutate(n_segs_utt = n())
}

# Create a blacklist of segment ids with too few pitch estimates to do loess
flag_too_few_pitch <- function(d, min_n_samples) {
  seg_id_blacklist <- d %>% 
    filter(!is.na(pitch)) %>% 
    count(seg_id) %>% 
    filter(n <= min_n_samples) %>% 
    pull(seg_id)
}

# fits polynomial function of time predicting interpolated, normalized log(pitch) values
# returns a vector with length 3 that contains the coeficients for re-creating polynomial 
fit_poly <- function(df, degree, fix = F, xfix, yfix) {
  
  if (fix == T) {
    coefs <- pracma::polyfix(x = df$time_wrt_bin, y = df$z_log_pitch_interp, n = degree,
                             xfix = xfix,
                             yfix = yfix)
  } else {
    coefs <- pracma::polyfit(x = df$time_wrt_bin, y = df$z_log_pitch_interp, n = degree)
  }
  
  tibble(
    coef_quadratic = coefs[1],
    coef_linear = coefs[2],
    coef_intercept = coefs[3]
  )
}

## take second order poly coefs (intercept, linear, quadratic terms) and generates a shape
predict_poly <- function(d) {
  p <- c(d$coef_quadratic, d$coef_linear, d$coef_intercept)
  
  tibble(
    time_ms = seq(10, 100, by = 10),
    pred = pracma::polyval(p, time_ms)
  ) 
}