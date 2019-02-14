## Tidy metadata files
## Read in recordings and participant metadata
## Cleans and creates a tidy data frame 

library(magrittr)
source(here::here("code/helpers.R"))
library(tidyverse)

# set up paths
metadata_path <- "data/00_metadata/Warlaumont"

# read excel metadata
# first, read the participant information
d_demo <- readxl::read_excel(path = here::here(metadata_path, "0Warlaumont.xlsx"),
                             sheet = "Participants",
                             skip = 1)

# next, we read the recordings information to get the audio file id
# linked with the participant demographic data
d_recordings <-  readxl::read_excel(path = here::here(metadata_path, "0Warlaumont.xlsx"),
                                    sheet = "Recordings",
                                    trim_ws = TRUE,
                                    skip = 1) 

# tidy variable names
d_demo %<>% tidy_var_names()
d_recordings %<>% tidy_var_names()

# select only variables of interest 
# separate names of recordings that were split into different files for one age point
# gather to convert data to long format 
d_recordings %<>% 
  rename(child_age_months = child_age_in_months) %>% 
  select(recording_filename, child_id, child_age_months) %>% 
  separate(col = recording_filename,
           sep = " and ",
           into = c("recording_filename_a", "recording_filename_b", "recording_filename_c")) %>% 
  gather(key = recording_type, value = recording_filename, -child_age_months, -child_id,
         na.rm = TRUE)

# select only variables of interest from subject metadata and clean child_id var name 
# to join data together

# TODO: ask AW about the rest of this metadata
d_demo %<>% 
  rename(child_id = x4_character_child_id) %>% 
  select(child_id, permission_level, 
         socioeconomic_status:language_input)

# join the recording and participant metadata 
d_meta_final <- left_join(d_recordings, d_demo, by = "child_id")

# save output
write_csv(d_meta_final, here::here("data/02_processed_data/act-lena-metadata.csv"))