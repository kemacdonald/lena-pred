## Tidy ITS files
## Read in ITS and participant metadata
## Cleans, joins, and saves a tidy data frame ready for analysis 
## Note that this pipeline only works for a single corpus of ITS files

library(magrittr)
library(rlena)
source(here::here("code/helpers.R"))
library(tidyverse)

# set up paths
its_path <- "data/00_metadata/Warlaumont"
metadata_path = "data/02_processed_data/"

# read metadata file
d_meta <- read_csv(here::here(metadata_path, "act-lena-metadata.csv")) %>% 
  select(child_id, child_age_in_months, recording_filename, 
         permission_level:language_input,
         -childs_gender) %>% 
  distinct()

# get all ITS file paths
all_files <- dir(path = its_path,
             pattern = "*.its",
             full.names = T,
             recursive = T)

# map the process ITS file function over all files
files_to_process <- all_files[1]
d <- files_to_process %>% purrr::map(safe_process_its_file)

# extract the errors and join with file name
error_log <- tibble (
  file_name = files_to_process %>% purrr::map_chr(extract_its_filename),
  error_message = transpose(d)[["error"]] %>% as.character()
)

# flatten list of data frames into one data frame
d_final <- transpose(d)[['result']] %>% reduce(bind_rows)

# join segments data with the rest of the metadata about the child (demographics)
d_final %<>% left_join(d_meta, by = c("child_id", "child_age_in_months"))

d_final %<>% 
  rename(mom_age = mothers_age_at_the_time_of_the_childs_3_month_recording,
         dad_age = fathers_age_at_the_time_of_the_childs_3_month_recording)

# save data to fst file for fast read/write
fst::write.fst(d_final, here::here("data/02_processed_data/act-lena-segments.fst"))

# save error log to review the ITS files later
write_csv(error_log, path = here::here("data/02_processed_data/act-lena-error-log.csv"))