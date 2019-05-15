######### Extract pitch tracks in batch ###########

### Libraries and Helpers -----------------------------------------------------------------
source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_helper_functions/pitch_helpers.R"))
source(here("code/00_helper_functions/lstm-helpers.R"))

### Extraction parameters from config file -----------------------------------------------
source(here("code/00_config/lena-pred-pitch-config.R"))
write_path <- paste0("data/03_summaries/", data_set)

### Extract pitch tracks -------------------------------------------------------------------
d <- dataset_config$files_to_analyze %>% map_df(get_pitch_contour, pitch_detect_config)

# store the total number of audio files 
total_files_in_folder <- dataset_config$files_to_analyze %>% length()

### Filter to first and last voiced frames 
batch_filter_voiced(d) -> d

# log transform pitch data
d %>% mutate(log_pitch = log(pitch)) -> d

# Create a blacklist of segment ids with too few pitch estimate 
# to do reliable loess interpolation and filter data
seg_id_blacklist <- flag_too_few_pitch(d, loess_config$min_n_samples_loess) 
d %>% filter(!(seg_id %in% seg_id_blacklist)) -> d

# store the number of audio files after filtering
total_files_after_filter <- d$seg_id %>% unique() %>% length()

### Batch interpolate -------------------------------------------------------------------
batch_interpolate(d, loess_config) -> d_interp  

# scale the log interpolated pitch values
d_interp %>% mutate(z_log_pitch_interp = scale(log_pitch_interp)) -> d_interp

### Temporal segmentation -------------------------------------------------------------------
# Divide each audio clip into fixed frame 100ms segments.
d_interp %>%
  create_time_bins(bin_width = time_filter_config$time_bin_width) %>%
  get_time_in_bin(sample_rate = loess_config$preds_sample_rate) %>%
  relabel_bins() -> d_interp

# Remove 100 ms segments with fewer than the min number of samples in each bin.
d_interp %>% filter(n_bins_in_seg == time_filter_config$min_samples_bin) -> d_interp

### Save outputs ----------------------------------------------------------------------------
write(seg_id_blacklist, here(write_path, "lena-pred-seg-blacklist.txt"))
write_fst(d_interp, here(write_path, "lena-pred-pitch-vals.fst"))