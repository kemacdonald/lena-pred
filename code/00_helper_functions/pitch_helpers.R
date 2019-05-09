##### Pitch processing helper functions 

# function to process and extract pitch contour from .wav file
# returns a data frame with the segment id, dataset, pitch values, and amplitude
get_pitch_contour <- function(file_path, p_config) {
  file_path_spl <- str_split(file_path, "/", simplify = T)
  
  d_out <- analyze(x = file_path, plot = FALSE,
                   pitchFloor = p_config$pitch_min,
                   pitchCeiling = p_config$pitch_max,
                   silence = p_config$silence_min,
                   autocorThres = p_config$autocor_threshold,
                   pathfinding = p_config$pathfinding_alg,
                   pitchMethods = p_config$pitch_methods, entropyThres = p_config$ent_threshold,
                   step = p_config$step_size,
                   wn = p_config$window_type,
                   windowLength = p_config$window_length)
  
  tidy_seg_meta(d_out, file_path, file_path_spl)
}

# tidy audio file metadata based on checking the dataset in the file path
# returns a data frame with appropriate metadata extracted from file path
tidy_seg_meta <- function(d, file_path, file_path_spl) {
  
  if ( str_detect(file_path, "pilot") ) {
    d %>%
      mutate(dataset = file_path_spl[10],
             path_to_wav = file_path,
             speech_register = file_path_spl[11],
             word_category = NA,
             seg_id = str_remove(file_path_spl[12], '.wav')) %>%
      select(seg_id, dataset, speech_register, word_category, pitch, voiced, time, ampl, path_to_wav)
  } else if ( str_detect(file_path, "ManyBabies") ) {
    d %>%
      mutate(dataset = str_remove(file_path_spl[9], "-norm"),
             path_to_wav = file_path,
             speech_register = file_path_spl[10],
             word_category = file_path_spl[11],
             seg_id = str_remove(file_path_spl[12], '.wav')) %>%
      select(seg_id, dataset, speech_register, word_category, pitch, voiced, time, ampl, path_to_wav)
  } else if ( str_detect(file_path, "IDSLabel") ) {
    d %>%
      mutate(dataset = file_path_spl[10],
             path_to_wav = file_path,
             speech_register = file_path_spl[11],
             word_category = NA,
             seg_id = str_remove(file_path_spl[12], '.wav')) %>%
      select(seg_id, dataset, speech_register, word_category, pitch, voiced, time, ampl, path_to_wav)
  } else {
    print("invalid specification of dataset in path_to_wav in config file")
  }
}

# embed audio player in html knit from Rmd
html_tag_audio <- function(file, type = "wav") {
  htmltools::tags$audio(controls = NA,
                        htmltools::tags$source(src = file,
                                               type = glue::glue("audio/{type}",
                                                                 type = type)))
}

# filters all segments in a dataframe, removing samples before and after 
# the first pitch candidate
# also re-zeroes the time variable to start at zero
batch_filter_voiced <- function(d) {
  d %>% 
    split(.$seg_id) %>% 
    map_df(filter_voiced)
}

filter_voiced <- function(d) {
  min_cut_point <- d %>% filter(!is.na(pitch)) %>% pull(time) %>% min()
  max_cut_point <- d %>% filter(!is.na(pitch)) %>%  pull(time) %>% max()
  
  d %>% 
    filter(time >= min_cut_point & time <= max_cut_point) %>% 
    mutate(time = time - min_cut_point)
}

# use loess model to intrpolate between pitch estimates
batch_interpolate <- function(d, loess_config, seg_id_blacklist) {
  d %>% 
    filter(!(seg_id %in% seg_id_blacklist)) %>% 
    split(.$seg_id) %>% 
    map_df(interpolate_loess, 
           frac_points = loess_config$frac_points_loess, 
           sample_rate = loess_config$preds_sample_rate) %>% 
    filter(!is.na(log_pitch_interp)) 
}

interpolate_loess <- function(d, sample_rate, frac_points) {
  
  #t_to_predict <- seq(0, max(d$time), by = sample_rate)
  t_to_predict <- d$time
  
  preds <- loess(log_pitch ~ time, 
                 data = d, 
                 span = frac_points) %>% 
    predict(t_to_predict) 
  
  tibble(
    seg_id = d$seg_id[1],
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
    left_join(d, .)
}

# returns kmeans cluster assignments for each 100 ms time bin
get_cluster_assignments <- function(df, k, scale_coefs = TRUE) {
  if (scale_coefs) {
    m_linear <- df$coef_linear %>% mean()
    m_quad <- df$coef_quadratic %>% mean()
    sd_linear <- df$coef_linear %>% sd()
    sd_quad <- df$coef_quadratic %>% sd()
    
    coefs_mat <- df %>% 
      select(coef_quadratic, coef_linear) %>% 
      as.matrix() %>% 
      scale()
  } else {
    coefs_mat <- df %>% 
      select(coef_quadratic, coef_linear) %>% 
      as.matrix()  
  }
  
  cl <- kmeans(coefs_mat, centers = k)
  
  list(centers = cl$centers %>% as_tibble(rownames = "cluster") %>% 
         mutate(coef_intercept = 0,  cluster = as.numeric(cluster),
                coef_linear_unscaled = coef_linear * sd_linear + m_linear,
                coef_quadratic_unscaled = coef_quadratic * sd_quad + m_quad),
       d_clusters = df %>% mutate(cluster = cl$cluster) )
}

# add the duration of utterances to the dataframe
add_utt_duration_segs <- function(d) {
  d %>% 
    group_by(seg_id) %>% 
    mutate(n_segs_utt = n())
}

# plot the polynomial shapes indicated by the cluster centers after kmeans step
# the sequence of these shapes is what the dnn is trying to learn
plot_cluster_shapes <- function(df_centers, scaled = TRUE) {
  if (scaled) {
    ms <- df_centers %>% 
      mutate(coef_linear = coef_linear_unscaled,
             coef_quadratic = coef_quadratic_unscaled) %>% 
      group_by(cluster) %>% 
      nest() %>% 
      mutate(poly_preds = map(data, predict_poly)) %>% 
      unnest(poly_preds)
  } else {
    ms <- df_centers %>% 
      group_by(cluster) %>% 
      nest() %>% 
      mutate(poly_preds = map(data, predict_poly)) %>% 
      unnest(poly_preds)  
  }
  
  
  if (ms$cluster %>% unique() %>% length <= 12) {
    ms %>% 
      ggplot(aes(x = time_ms, y = pred, color = as_factor(cluster))) +
      geom_line(size = 1) +
      ggrepel::geom_label_repel(aes(label = cluster, fill = as_factor(cluster)), 
                                data = filter(ms, time_ms == max(time_ms)),
                                color = "white",
                                box.padding = unit(0.35, "lines"),
                                size = 3,
                                nudge_x = 2) +
      guides(fill = F, color = F) +
      ggthemes::scale_color_ptol(drop = FALSE) +
      ggthemes::scale_fill_ptol(drop = FALSE)
  } else {
    ms %>% 
      ggplot(aes(x = time_ms, y = pred)) +
      geom_line(size = 1) +
      ggrepel::geom_label_repel(aes(label = cluster), 
                                data = filter(ms, time_ms == max(time_ms)),
                                color = "black",
                                box.padding = unit(0.35, "lines"),
                                size = 3,
                                nudge_x = 2) +
      guides(fill = F, color = F) +
      facet_wrap(~cluster)
  }
}

plot_clusters_scatter <- function(d) {
  if (d$cluster %>% unique() %>% length() <= 12) {
    d_final$d_clusters %>% 
      ggplot(aes(coef_quadratic, coef_linear, color = as_factor(cluster))) +
      geom_point(size = 3, alpha = 0.7) +
      ggthemes::scale_color_ptol(drop = FALSE) +
      facet_grid(dataset ~ speech_register)
  } else {
    d %>% 
      ggplot(aes(coef_quadratic, coef_linear, color = cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      facet_grid(dataset ~ speech_register)
  }
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

# takes a seg_id index, df with original pitch contour values, and df with pitch recongstruced from
# separte 100ms second order polynomials and plots them alongside each other for sanity check
plot_reconstructed_pitch <- function(seg_id_to_plot, df_raw, df_preds) {
  
  # get lims for plot
  buffer <- 0.2
  ylims <- c(min(df_raw$z_log_pitch_interp) - buffer, max(df_raw$z_log_pitch_interp) + buffer)
  raw_pitch_max <- max(df_raw$pitch_original) + 100
  # plot raw pitch contour
  orig_raw_pitch <- df_raw %>%
    filter(seg_id == seg_id_to_plot) %>%
    group_by(seg_id) %>%
    mutate(n_samples = n(),
           x = seq(0, unique(n_samples) - 1, by = 1)) %>%
    ggplot(aes(time, pitch_original)) +
    geom_point(size = 2, color = "#756bb1") +
    #geom_line(size = 1, color = "#756bb1") +
    guides(fill = F) +
    lims(y = c(0, raw_pitch_max)) +
    labs(x = "time (ms)", y = "freq (Hz)") +
    facet_wrap(~seg_id, scales = "free_x") +
    theme(legend.position = 'top') 
  
  
  # plot interpolated pitch contour
  orig <- df_raw %>%
    filter(seg_id == seg_id_to_plot) %>%
    group_by(seg_id) %>%
    mutate(n_samples = n(),
           x = seq(0, unique(n_samples) - 1, by = 1)) %>%
    ggplot(aes(time, z_log_pitch_interp)) +
    geom_line(size = 1, color = "#756bb1") +
    guides(fill = F) +
    lims(y = ylims) +
    labs(x = "time (ms)", y = "normalized log pitch") +
    facet_wrap(~seg_id, scales = "free_x") +
    theme(legend.position = 'top') 
  
  # create data for plotting segmented pitch contour with shape category
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
  
  # make segmented pitch plot
  segmented_plot <-  df_preds_expanded %>%
    ggplot(aes(time_ms, pred)) +
    geom_line(size = 1, color = "#756bb1") +
    lims(y = ylims) +
    guides(fill = F) +
    facet_wrap(~time_bin_id, scales = "free_x", nrow =1) +
    theme(legend.position = 'top',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  if ( df_cluster_labels$cluster %>% nlevels() <= 12 ) {
    segmented_plot <- segmented_plot +
      geom_label(data = df_cluster_labels,
                 aes(time_ms, pred, label = cluster,fill = cluster),
                 color = "white") +
      ggthemes::scale_fill_ptol(drop = FALSE)
  } else {
    segmented_plot <- segmented_plot +
      geom_label(data = df_cluster_labels,
                 aes(time_ms, pred, label = cluster),
                 color = "black") 
  }
  
  cowplot::plot_grid(orig_raw_pitch, orig, segmented_plot, nrow = 3)
  
}

# Create a blacklist of segment ids with too few pitch estimates to do loess
flag_too_few_pitch <- function(d, min_n_samples) {
  seg_id_blacklist <- d %>% 
    filter(!is.na(pitch)) %>% 
    count(seg_id) %>% 
    filter(n <= min_n_samples) %>% 
    pull(seg_id)
}
