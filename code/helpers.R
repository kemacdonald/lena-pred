###### helper functions ######

## reading and tidying data
tidy_var_name <- function(var_name) {
  # remove everything after first period
  gsub( "\\. .*$", "", var_name )
}

tidy_var_names <- function(df) {
  names(df) <- names(df) %>% purrr::map(tidy_var_name)
  df %>% janitor::clean_names()
}


## reading ITS files helpers

# some rather opaque regex to extract ids and file names from paths
# with different structures. I borrowed the gsub code from 
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r

check_filename_type <- function(file_name) {
  split_f_name <- str_split(file_name, pattern = "/", simplify = T) 
  if ( str_detect(split_f_name, pattern = "0its") %>% sum() > 0 ) {
    TRUE
  } else {
    FALSE
  }
}

extract_id <- function(path_to_its) {
  gsub(".*/", "", path_to_its) %>% gsub("_.*$", "", .)
}

extract_its_filename <- function(file_path) {
  if ( check_filename_type(file_path) ) {
    gsub(".*its/", "", file_path)  
  } else {
    gsub(".*w0.*/", "", file_path)  
  }
}

process_spkr_variable <- function(df) {
  df %>% 
    mutate(segment_type = case_when(
    spkr == "SIL" ~ "silence",
    str_detect(spkr, "NO+") ~ "noise",
    str_detect(spkr, "OL+") ~ "overlap",
    str_detect(spkr, "TV+") ~ "electronic",
    TRUE ~ "vocalization"
  ),
  speaker = case_when(
    str_detect(spkr, "FA+") ~ "female_adult",
    str_detect(spkr, "MA+") ~ "male_adult",
    str_detect(spkr, "CH+") ~ "key_child",
    str_detect(spkr, "CX+") ~ "other_child",
  ),
  spkr_gender = case_when(
    str_detect(spkr, "FA+") ~ "female",
    str_detect(spkr, "MA+") ~ "male",
    TRUE ~ "NA"
  ),
  spkr_age = case_when(
    str_detect(spkr, "FA+") ~ "adult",
    str_detect(spkr, "MA+") ~ "adult",
    str_detect(spkr, "CH+") ~ "child",
    str_detect(spkr, "CX+") ~ "child",
    TRUE ~ "NA"
  ),
  child_role = case_when(
    str_detect(spkr, "CH+") ~ "key_child",
    str_detect(spkr, "CX+") ~ "other_child",
    TRUE ~ "NA"
  ),
  segment_distance = case_when(
    str_detect(spkr, ".+N") ~ "near",
    str_detect(spkr, ".+F") ~ "faint",
    TRUE ~ "NA"
  )) %>% 
    rename(old_spkr_encoding = spkr)
}

# Note that there are two types of Vocalization Activity Block that include the
# key child, an adult, and another child (XIC & XIOCAC). The block types differ
# by whether or not Conversational Turns were produced in the block. When key
# child vocalizations and adult speech are contiguous, Conversational Turns may
# be produced, but when vocalizations from the other child intercede between
# key child and adult segments, Conversational Turns are not produced. 

process_conv_type_var <- function(df) {
  df %>% 
    mutate(conv_n_participants = case_when(
      convType %in% c("AMF", "AMM", "XM", "CM") ~ 1,
      convType %in% c("AICF", "AIOCF", "AICM", "AIOCM", "XIOCC", "XIOCA", "CIC", "CIOCX") ~ 2,
      convType %in% c("CIOCAX", "AIOCCXF", "AIOCCXM", "XIOCAC", "XIC") ~ 3
    ),
    conv_init = case_when(
      str_detect(convType, "^C.") ~ "child",
      str_detect(convType, "^X.") ~ "child",
      str_detect(convType, "^A.") ~ "adult",
    ),
    conv_participant_2 = case_when(
      convType %in% c("AICF", "AIOCCXF", "AIOCCXM", "XIOCC", "XIC", "XIOCAC") ~ "key_child",
      convType %in% c("CIOCX", "AIOCF", "AIOCM") ~ "other_child",
      convType %in% c("CIC", "CIOCAX", "XIOCA") ~ "adult",
    ),
    conv_participant_3 = case_when(
      convType %in% c("AIOCCXF", "AIOCCXM", "CIOCAX") ~ "other_child",
      convType %in% c("XIOCAC", "XIC", "CIOCAX") ~ "adult",
    ),
    conv_init_child_role = case_when(
      str_detect(convType, "^C.") ~ "key_child",
      str_detect(convType, "^X.") ~ "other_child"
    ),
    conv_init_gender = case_when(
      str_detect(convType, ".{2}F") ~ "female",
      str_detect(convType, ".{2}M") ~ "male"
    ),
    conv_has_turns = case_when(
      convType == "XIOCAC" ~ "no_turns",
      convType == "XIC" ~ "turns",
    )) %>% 
    rename(old_conv_type_encoding = convType)
}

process_its_file <- function(file_path) {
  d_its <- rlena::read_its_file(x = here::here(file_path))
  
  # get child id from file path
  extracted_child_id <- extract_id(file_path)

  # get the sub info
  sub_info_its <- gather_child_info(d_its) %>%
    select(Birthdate, ChronologicalAge, Gender) %>%
    mutate(child_id = extracted_child_id,
           n_recordings_file = gather_recordings(d_its) %>% nrow(),
           ChronologicalAge = readr::parse_number(ChronologicalAge),
           Gender = case_when(
             Gender == "M" ~ "male",
             Gender == "F" ~ "female",
             TRUE ~ Gender
           )) 
  
  # get all the segments, tidy the speaker category and conversation type variables
  d_output <- gather_segments(d_its) %>% 
    process_spkr_variable() %>% 
    process_conv_type_var() %>% 
    mutate(child_id = extracted_child_id,
           segment_length_sec = endTime - startTime,
           convFloorType = case_when(
             convFloorType == "FI" ~ "first_time_speaking",
             convFloorType == "FH" ~ "has_spoken_before",
           )) 

  # join metadata and tidy variable names
  d_output %>%
    left_join(sub_info_its, by = "child_id") %>%
    select(child_id, Birthdate, ChronologicalAge, everything()) %>%
    rename(child_age_in_months = ChronologicalAge,
           child_gender = Gender) %>% 
    janitor::clean_names() %>% 
    rename(peak_db = peak_d_b,
           average_db = average_d_b) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate(spkr_gender = ifelse(child_role == "key_child", child_gender, spkr_gender),
           conv_init_gender = ifelse(conv_init_child_role == "key_child", 
                                     child_gender, 
                                     conv_init_gender))
}

safe_process_its_file <- purrr::safely(process_its_file)
