##### Pitch processing helper functions 

# embed audio player in html knit from Rmd
html_tag_audio <- function(file, type = "wav") {
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

# use loess model to intrpolate between pitch estimates
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
    mutate(time_bin_id = seq_along(time_bin)) %>% 
    left_join(d, .)
}

get_cluster_assignments <- function(df, k) {
  
  coefs_mat <- df %>% 
    select(coef_quadratic, coef_linear) %>% 
    as.matrix()  
  
  cl <- kmeans(coefs_mat, centers = k)
  
  list(centers = cl$centers %>% as_tibble(rownames = "cluster") %>% mutate(coef_intercept = 0, 
                                                                           cluster = as.numeric(cluster) - 1),
       d_clusters = df %>% mutate(cluster = cl$cluster - 1) 
  )
}

add_utt_duration_segs <- function(d) {
  d %>% 
    group_by(seg_id) %>% 
    mutate(n_segs_utt = n())
}

# plot the polynomial shapes indicated by the cluster centers after kmeans step
# the sequence of these shapes is what the dnn is trying to learn
plot_cluster_shapes <- function(df_centers) {
  ms <- df_centers %>% 
    group_by(cluster) %>% 
    nest() %>% 
    mutate(poly_preds = map(data, predict_poly)) %>% 
    unnest(poly_preds)
  
  ms %>% 
    ggplot(aes(x = time_ms, y = pred, color = as_factor(cluster))) +
    geom_line(size = 1) +
    ggrepel::geom_label_repel(aes(label = cluster, fill = as_factor(cluster)), 
                             data = filter(ms, time_ms == max(time_ms)),
                             color = "black",
                             box.padding = unit(0.35, "lines"),
                             size = 3,
                             nudge_x = 2) +
    guides(fill = F, color = F) +
    ggthemes::scale_color_ptol(drop = FALSE) +
    ggthemes::scale_fill_ptol(drop = FALSE)
    
}

# plot a sample of the pitch shapes based on the polynomial coefs returned in the
# fitting of second degree polynomial to each 100ms time bin
plot_sample_pitch_shapes <- function(d, frac_sample) {
  d %>% 
    group_by(cluster) %>% 
    sample_frac(frac_sample) %>% 
    group_by(cluster, time_bin_id) %>% 
    nest() -> d_clusters
  
  d_clusters %>% mutate(poly_preds = map(data, predict_poly)) -> d_clusters
  
  d_clusters %>% 
    unnest(poly_preds) %>% 
    ggplot(aes(x = time_ms, y = pred, group = time_bin_id)) +
    geom_line(alpha = 0.5, size = 1) +
    facet_wrap(~cluster, scales = "free_y")
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

## take second order poly coefs (intercept, linear, quadratic terms) and generates a shape
predict_poly <- function(d) {
  p <- c(d$coef_quadratic, d$coef_linear, d$coef_intercept)
  
  tibble(
    time_ms = seq(10, 100, by = 10),
    pred = pracma::polyval(p, time_ms)
  ) 
}

# takes a seg_id index, df with original pitch contour values, and df with pitch recongstruced from
# separte 100ms second order polynomials and plots them alongside each other for sanity check
plot_reconstructed_pitch <- function(seg_id_to_plot, df_raw, df_preds) {
  
  orig <- df_raw %>%
    filter(seg_id == seg_id_to_plot) %>%
    group_by(seg_id) %>%
    mutate(n_samples = n(),
           x = seq(0, unique(n_samples) - 1, by = 1)) %>%
    ggplot(aes(time, z_log_pitch)) +
    geom_line(size = 1, color = "#756bb1") +
    guides(fill = F) +
    labs(x = "time (ms)", y = "normalized log pitch") +
    facet_wrap(~seg_id, scales = "free_x") +
    theme(legend.position = 'top') 
  
  df_preds_expanded <- df_preds %>%
    mutate(cluster = as_factor(cluster)) %>% 
    filter(seg_id == seg_id_to_plot) %>%
    unnest(poly_preds) %>%
    group_by(seg_id) %>%
    mutate(n_samples = n(),
           x = seq(0, unique(n_samples) - 1, by = 1))
  
  df_cluster_labels <- df_preds_expanded %>%
    select(seg_id, time_ms, pred, time_bin_id, cluster) %>%
    mutate(pred = min(pred) + 0.3,
           time_ms = max(time_ms) * 0.2)
  
  segmented <-  df_preds_expanded %>%
    ggplot(aes(time_ms, pred)) +
    geom_line(size = 1, color = "#756bb1") +
    guides(fill = F) +
    geom_label(data = df_cluster_labels,
               aes(time_ms, pred, label = cluster, 
                   fill = cluster)) +
    facet_wrap(~time_bin_id, scales = "free_x", nrow =1) +
    theme(legend.position = 'top',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ggthemes::scale_fill_ptol(drop = FALSE)
  
  cowplot::plot_grid(orig, segmented, nrow = 2)
  
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
