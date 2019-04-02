##### Pitch processing helper functions 

html_tag_audio <- function(file,
                           type = "wav") {
  htmltools::tags$audio(controls = NA,
                        htmltools::tags$source(src = file,
                                               type = glue::glue("audio/{type}",
                                                                 type = type)))
}

# filters time series to first voiced sample
# re-zeroes the the time variable
filter_first_voiced <- function(df) {
  min_cut_point <- df %>% filter(!is.na(pitch)) %>% pull(time) %>% min()
  max_cut_point <- df %>% filter(!is.na(pitch)) %>%  pull(time) %>% max()
  
  df %>% filter(time >= min_cut_point & time <= max_cut_point) %>% mutate(time = time - min_cut_point)
}

interpolate_loess <- function(df, sample_rate, frac_points) {
  
  t_to_predict <- seq(0, max(df$time), by = sample_rate)
  preds <- loess(pitch ~ time, data = df, 
                 span = frac_points) %>% 
    predict(t_to_predict) 
  
  tibble(
    seg_id = df$seg_id[1],
    dataset = df$dataset[1],
    speech_register = df$speech_register[1],
    time = t_to_predict, 
    pitch_interpolated = preds
  ) 
}

# creates equally-spaced time bins for each clip
create_time_bins <- function(d, bin_width) {
  d_interp %>% 
    group_by(seg_id) %>% 
    mutate(time_bin = cut_width(time, 
                                width = bin_width,
                                closed = 'left',
                                boundary = 0))
}

# re-zero time wrt to this 100 ms time bin
get_time_in_bin <- function(d) {
  d %>% 
    group_by(seg_id, time_bin) %>% 
    mutate(time_wrt_bin = seq_along(time_bin) * preds_sample_rate,
           n_bins_in_seg = n())
}


# create more meaningful bin labels
relabel_bins <- function(d) {
  d %>% 
    distinct(seg_id, time_bin) %>% 
    group_by(seg_id) %>% 
    mutate(time_bin_num = seq_along(time_bin)) %>% 
    left_join(d, .)
}

get_cluster_assignments <- function(df, k) {
  
  coefs_mat <- df %>% 
    select(coef_quadratic, coef_linear) %>% 
    as.matrix()  
  
  cl <- kmeans(coefs_mat, centers = k)
  
  df %>% mutate(cluster = cl$cluster) 
  
}

## fits polynomial function of time predicting interpolated, normalized log(pitch) values
## returns a vector with length 3 that contains the coeficients for re-creating polynomial 
fit_poly <- function(df, degree, fix = F, xfix, yfix) {
  
  if (fix == T) {
    coefs <- pracma::polyfix(x = df$time_wrt_bin, y = df$z_log_pitch, n = degree,
                             xfix = xfix,
                             yfix = yfix)
  } else {
    coefs <- pracma::polyfit(x = df$time_wrt_bin, y = df$z_log_pitch, n = degree)
  }
  
  tibble(
    coef_quadratic = coefs[1],
    coef_linear = coefs[2],
    coef_intercept = coefs[3]
  )
}

predict_poly <- function(d) {
  p <- c(d$coef_quadratic, d$coef_linear, d$coef_intercept)
  
  tibble(
    time_ms = seq(10, 100, by = 10),
    pred = pracma::polyval(p, time_ms)
  ) 
}



## function to process and extract pitch contour from .wav file
## returns a data frame with the segment id, dataset, pitch values, and amplitude
get_pitch_contour <- function(file_path, ...) {
  file_path_spl <- str_split(file_path, "/", simplify = T)
  
  analyze(x = file_path,
          pitchFloor = lowest_pitch,
          pitchCeiling = highest_pitch,
          silence = silence_min,
          plot = FALSE) %>% 
    mutate(dataset = file_path_spl[10],
           path_to_wav = file_path,
           speech_register = file_path_spl[11], 
           seg_id = str_remove(file_path_spl[12], '.wav')) %>% 
    select(dataset, speech_register, seg_id, pitch, voiced, time, ampl, path_to_wav) 
}
