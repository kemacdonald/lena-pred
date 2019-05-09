# generating new theme
kyle_theme <- function(base_size = 16,
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
        size = 14,
        hjust = 0),
      plot.subtitle = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        size = 12,
        hjust = 0,
        vjust = 1),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      panel.grid.major = element_blank(),   
      panel.grid.minor = element_blank(),   
      
      complete = TRUE
    )
}

theme_set(kyle_theme())
