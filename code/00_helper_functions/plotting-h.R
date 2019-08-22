## Some plotting functions 

# generating new theme
kyle_theme <- function(base_size = 24,
                       base_family = "",
                       base_line_size = base_size / 170,
                       base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        size = rel(0.85),
        hjust = 0),
      plot.subtitle = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        size = rel(0.75),
        hjust = 0,
        vjust = 1),
      legend.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)
      ),
      legend.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.6)
      ),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.55)),
      panel.grid.major = element_blank(),   
      panel.grid.minor = element_blank(),   
      
      complete = TRUE
    )
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
                                nudge_x = 2,
                                nudge_y = -0.1) +
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
    d %>% 
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

# takes a seg_id index, df with original pitch contour values, and df with pitch recongstruced from
# separte 100ms second order polynomials and plots them alongside each other for sanity check
plot_reconstructed_pitch <- function(seg_id_to_plot, df_raw, df_preds) {
  
  # get lims for plot
  buffer <- 0.2
  norm_pitch_vals <- df_raw %>% filter(seg_id == seg_id_to_plot) %>% pull(z_log_pitch_interp)
  ylims <- c(min(norm_pitch_vals) - buffer, 
             max(norm_pitch_vals) + buffer)
  
  raw_pitch_max <- max(df_raw$pitch_original, na.rm = T) + 100
  
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
    lims(y = c(-2.5, 2.5)) +
    #lims(y = ylims) +
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
    #lims(y = ylims) +
    lims(y = c(-2.5, 2.5)) +
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
                 color = "white",
                 nudge_x = 20,
                 nudge_y = -0.2) +
      ggthemes::scale_fill_ptol(drop = FALSE)
  } else {
    segmented_plot <- segmented_plot +
      geom_label(data = df_cluster_labels,
                 aes(time_ms, pred, label = cluster),
                 color = "black",
                 nudge_x = 20,
                 nudge_y = -0.2) 
  }
  
  cowplot::plot_grid(orig_raw_pitch, orig, segmented_plot, nrow = 3)
  
}

plot_model_output <- function(d, col_name, y_axis_lab) {
  ms1 <- d %>% 
    group_by(speech_register, seg_id) %>% 
    summarise(m = mean( {{ col_name }} )) %>% 
    tidyboot_mean(column = m) 
  
  y_upper_a <- max(ms1$empirical_stat)
  
  a <- ms1 %>%
    ggplot(aes(x = speech_register, y = empirical_stat)) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
    ylim(0, y_upper_a + .1) +
    labs(x = "Speech Register", y = y_axis_lab)
  
  ms2 <- d %>%
    group_by(speech_register, n_qshapes) %>%
    tidyboot_mean(column = {{ col_name }} )
  
  y_upper_b <-max(ms2$empirical_stat)
  
  b <- ms2 %>%
    ggplot(aes(x = as_factor(n_qshapes), y = empirical_stat, color = speech_register)) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
    lims(y=c(0, y_upper_b + 0.1)) +
    labs(x = "Number of Q-shapes", y = y_axis_lab, color = "Speech Register") +
    scale_color_ptol() +
    theme(legend.position = 'top')
  
  plot_grid(a, b, scale = c(0.8, 0.8), rel_widths = c(0.7, 1))
}
