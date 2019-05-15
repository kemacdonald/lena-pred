### Fit poly and cluster -------------------------------------------------------------

# Setup
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch_helpers.R"))
source(here("code/00_config/lena-pred-pitch-config.R"))
path <- paste0("data/03_summaries/", data_set)

# Read interpolated pitch values for each segment
d <- read_fst(here(path, "lena-pred-pitch-vals.fst"))

# fix issue with speaker 126 -> should be 1266
d <- d %>% mutate(speaker_id = ifelse(speaker_id == "126", "1266", speaker_id))

# Add the duration of segments in ms for clip
d <- d %>% group_by(seg_id) %>% mutate(duration_ms = max(time))

# Fit second-order polynomial in each time bin
d %>%
  group_by(seg_id, dataset, speech_register, speaker_id,
           time_bin_id, duration_ms, speaker_id) %>%
  nest() -> d_by_bin

# fit polynomial and make predictions based on fit
d_by_bin %>%
  mutate(poly_coefs = map(data, fit_poly, poly_fit_config$degree_poly)) %>%
  mutate(poly_preds = map(poly_coefs, predict_poly)) -> d_by_bin

# Kmeans clustering of poly coefs
d_coefs <- unnest(d_by_bin, poly_coefs, .drop = T)
d_final <- map_get_clusters(poly_fit_config$n_q_shapes, d = d_coefs, iter_max = 20, scale_coefs = TRUE) 

# add one set of cluster assignments to nested data (for viz purposes later on)
d_tmp <- d_final[[2]]
d_by_bin %>% left_join(select(d_tmp$d_clusters, seg_id, time_bin_id, cluster),
                       by = c("seg_id", "time_bin_id")) -> d_by_bin

### Save outputs ----------------------------------------------------------------------------
write_rds(d_final, here(path, "lena-pred-kmeans-outputs.rds"), compress = "gz")
write_rds(d_by_bin, here(path, "lena-pred-nested-pitch-vals.rds"), compress = "gz")
