# Extract pitch tracks in batch  ------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch-extraction-h.R"))
source(here("code/00_config/lena-pred-config.R"))
write_path <- paste(config_obj$paths_config$pitch_sum_path, sep = "/")
plan(multiprocess)

# Extract pitch tracks ----------------------------------------------------

d <- config_obj$paths_config$files_to_analyze %>% 
  future_map_dfr(get_pitch_contour, 
                 config_obj$pitch_detect_config, 
                 .progress = TRUE)  

batch_filter_voiced(d) -> d # Filter to first and last voiced frames 
d %>% mutate(log_pitch = log(pitch)) -> d # log transform pitch data

# Create a blacklist of segment ids with too few pitch estimate 
# to do reliable loess interpolation and filter data
seg_id_blacklist <- flag_too_few_pitch(d, config_obj$loess_config$min_n_samples_loess) 
d %>% filter(!(seg_id %in% seg_id_blacklist)) -> d

# Save outputs ------------------------------------------------------------
write(seg_id_blacklist, here(write_path, paste0("lena-pred-seg-blacklist-", config_obj$exp_config$dataset, ".txt")))

write_rds(d, here(write_path, 
                  paste0("lena-pred-pitch-vals-pre-interp-", config_obj$exp_config$dataset, ".rds")),
          compress = "gz")

print("Completed pitch extraction")
