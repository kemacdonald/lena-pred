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

extract_id <- function(path_to_its) {
  gsub(".*/", "", path_to_its) %>% gsub("_.*$", "", .)
}

extract_its_filename <- function(file_path) {
  gsub(".*its/", "", file_path)
}

process_its_file <- function(file_path) {
  d_its <- rlena::read_its_file(x = here::here(file_path))
  
  # get child id from file path
  child_id <- extract_id(file_path)

  # get the sub info
  sub_info_its <- gather_child_info(d_its) %>%
    select(Birthdate, ChronologicalAge) %>%
    mutate(child_id = child_id,
           n_recordings_file = gather_recordings(d_its) %>% nrow(),
           ChronologicalAge = readr::parse_number(ChronologicalAge))
  # get all the segments
  d_output <- gather_segments(d_its) %>% mutate(child_id = child_id)

  # join metadata and tidy variable names
  d_output %>%
    left_join(sub_info_its, by = "child_id") %>%
    select(child_id, Birthdate, ChronologicalAge, everything()) %>%
    rename(child_age_in_months = ChronologicalAge) %>% 
    janitor::clean_names()
}

safe_process_its_file <- purrr::safely(process_its_file)
