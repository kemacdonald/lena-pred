# Extract pitch tracks in batch  ------------------------------------------

# Libraries and Helpers  --------------------------------------------------

source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch-extraction-h.R"))

# Extraction parameters from config file ----------------------------------
source(here("code/00_config/lena-pred-pitch-config.R"))
write_path <- paste(paths_config$pitch_sum_path, sep = "/")

plan(multiprocess)

# Extract pitch tracks ----------------------------------------------------

# Paralleize the pitch extraction using furrr's future_map
d <- paths_config$files_to_analyze %>% 
  future_map_dfr(get_pitch_contour, 
                 pitch_detect_config, 
                 .progress = TRUE)  

# Filter to first and last voiced frames 
batch_filter_voiced(d) -> d

# log transform pitch data
d %>% mutate(log_pitch = log(pitch)) -> d

# Create a blacklist of segment ids with too few pitch estimate 
# to do reliable loess interpolation and filter data
seg_id_blacklist <- flag_too_few_pitch(d, loess_config$min_n_samples_loess) 
d %>% filter(!(seg_id %in% seg_id_blacklist)) -> d

# Save outputs ------------------------------------------------------------
write(seg_id_blacklist, here(write_path, "lena-pred-seg-blacklist.txt"))
write_rds(d, here(write_path, "lena-pred-pitch-vals-pre-interp.rds"))


print("Completed pitch extraction")