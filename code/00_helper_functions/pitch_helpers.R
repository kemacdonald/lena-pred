##### Pitch processing helper functions ------------

# filters time series to first voiced sample
# re-zeroes the the time variable
filter_first_voiced <- function(df) {
  min_cut_point <- df %>% 
    filter(!is.na(pitch)) %>% 
    pull(time) %>% min()
  
  df %>% filter(time >= min_cut_point) %>% mutate(time = time - min_cut_point)
}


interpolate_loess <- function(df, sample_rate, frac_points) {
  t_to_predict <- seq(0, max(df$time), by = sample_rate)
  preds <- loess(pitch ~ time, data = df, 
                 span = frac_points) %>% 
    predict(t_to_predict) 
  
  tibble(
    seg_id = df$seg_id[1],
    time = t_to_predict, 
    pitch_interpolated = preds
  ) 
}

get_cluster_assignments <- function(df, k) {
  
  coefs_mat <- df %>% 
    select(coef2, coef3) %>% 
    as.matrix()  
  
  cl <- kmeans(coefs_mat, centers = k)
  
  df %>% mutate(cluster = cl$cluster %>% as.factor()) 
  
}

## fits polynomial function of time predicting interpolated, normalized log(pitch) values
## returns a vector with length 3 that contains the coeficients for re-creating polynomial 
fit_poly <- function(df, degree) {
  
  coefs <- pracma::polyfit(x = df$time_wrt_bin, y = df$z_log_pitch, n = degree)
  
  tibble(
    coef1 = coefs[1],
    coef2 = coefs[2],
    coef3 = coefs[3]
  )
}

predict_poly <- function(d) {
  p <- c(d$coef1, d$coef2, d$coef3)
  
  tibble(
    time_ms = seq(0, 100, by = 10),
    pred = pracma::polyval(p, time_ms)
  ) 
}



## function to process and extract pitch contour from .wav file
# returns a data frame with the segment id, dataset, pitch values, and amplitude
get_pitch_contour <- function(file_path, ...) {
  file_path_spl <- str_split(file_path, "/", simplify = T)
  
  analyze(x = file_path,
          pitchFloor = lowest_pitch,
          pitchCeiling = highest_pitch,
          plot = FALSE) %>% 
    mutate(dataset = file_path_spl[10],
           seg_id = str_remove(file_path_spl[11], '.wav')) %>% 
    select(dataset, seg_id, pitch, voiced, time, ampl) 
}