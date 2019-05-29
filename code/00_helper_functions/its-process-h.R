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
  new_vars <- df %>% 
    filter(!is.na(convType)) %>% # this makes sure we don't get duplicates within a conversation block
    distinct(blkId, convType) %>% 
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
      str_detect(convType, "^X.") ~ "other_child",
      str_detect(convType, "^C.") ~ "key_child",
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
  
  df %>% left_join(new_vars)
  
}

process_its_file <- function(file_path, data_granularity = "conversation") {
  d_its <- rlena::read_its_file(x = here::here(file_path))
  extracted_child_id <- extract_id(file_path)
  sub_info_its <- get_sub_info_its(d_its, extracted_child_id)
  
  if (data_granularity == "segment") {
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
    
    # join metadata and tidy data frame names
    d_output %>% 
      left_join(sub_info_its, by = "child_id") %>% 
      tidy_its_df()
    
  } else if (data_granularity == "block") {
    d_output <- rlena::gather_blocks(d_its) %>% mutate(child_id = extracted_child_id) 
    
    d_output %>% 
      left_join(sub_info_its, by = "child_id") %>% 
      select(child_id, Birthdate, ChronologicalAge, everything()) %>%
      rename(child_age_months = ChronologicalAge,
             child_gender = Gender) %>% 
      janitor::clean_names() %>% 
      rename(peak_db = peak_d_b,
             average_db = average_d_b) %>% 
      mutate_if(is.character, str_to_lower) 
  }
}


tidy_its_df <- function(d) {
  d %>%
    select(child_id, Birthdate, ChronologicalAge, everything()) %>%
    rename(child_age_months = ChronologicalAge,
           child_gender = Gender) %>% 
    janitor::clean_names() %>% 
    rename(peak_db = peak_d_b,
           average_db = average_d_b) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate(spkr_gender = ifelse(child_role == "key_child", child_gender, spkr_gender),
           conv_init_gender = case_when(
             conv_init == "adult" ~ conv_init_gender, 
             conv_init == "child" & conv_init_child_role == "key_child" ~ child_gender
           ))
}

# get the sub info
get_sub_info_its <- function(d_its, extracted_child_id) {
  gather_child_info(d_its) %>%
    select(Birthdate, ChronologicalAge, Gender) %>%
    mutate(child_id = extracted_child_id,
           n_recordings_file = gather_recordings(d_its) %>% nrow(),
           ChronologicalAge = readr::parse_number(ChronologicalAge),
           Gender = case_when(
             Gender == "M" ~ "male",
             Gender == "F" ~ "female",
             TRUE ~ Gender
           )) 
}

safe_process_its_file <- purrr::safely(process_its_file)

# converts a date time object to a date time object with the same day
# this is useful if all you care about is the time of day of something happening
convert_date <- function(old_date) {
  format(old_date, format = "%H:%M:%S") %>% as.POSIXct(format = "%H:%M:%S")  
}


## get the sound type with the longest time within a activity block 
## as coded in the ITS file
get_longest_sound_type <- function(codes, time_lengths) {
  max_length_code <- codes[which(time_lengths == max(time_lengths))] 
  
  if ( length(max_length_code) > 1)  {
    max_length_code[sample(1:length(max_length_code), size = 1)]
  } else {
    max_length_code
  }
}


## detect whether there is a specific speaker present in a conversational block
is_spkr_present <- function(conv_vect, test_string) {
  if ( str_detect(conv_vect, test_string) %>% sum(na.rm = T) > 0 ) {
    "present"
  } else {
    "absent"
  }
}


## compute length of a LENA conversational block
get_conv_length <- function(start_time_vect, end_time_vect) {
  max(end_time_vect) - min(start_time_vect) 
}


## functions to expand the sound activity events within 
## a LENA conversation
expand_conv <- function(conv_df, samp_rate) {
  conv_expanded <- conv_df %>% 
    pmap(data.frame) %>% 
    purrr::map_dfr(expand_utt, samp_rate = samp_rate)
  
  # start each conversation at zero
  conv_expanded %>% mutate(time_sec = time_sec - min(time_sec))
}

expand_utt <- function(utt_row_df, samp_rate) {
  # segment_type <- utt_row_df %>% pull(segment_type)
  # blk_id <- utt_row_df %>% pull(blk_id)
  time_sec <- seq(utt_row_df$start_time, utt_row_df$end_time, by = samp_rate)  
  
  tibble(
    child_id = utt_row_df$child_id,
    blk_id = utt_row_df$blk_id,
    segment_type = utt_row_df$segment_type,
    time_sec = time_sec,
  )
}
