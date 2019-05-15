#### Testing helpers -------------------------------------------------

# prop train/test--------------------------------------------------------
# check that the train and test sets have the correct proportion of 
# audio segments

check_train_test_sampling <- function(d, total_segs, prop_train) {
  n_segs_train <- d$train_data$next_cluster_seg_id %>% 
    unique() %>% 
    length()
  
  if( n_segs_train == as.integer(total_segs * prop_train) )  {
    print(paste("test passed, there are", as.integer(total_segs * dnn_dataset_config$prop_train), 
                "utterances in the training dataset"))
  } else {
    print("test failed - check the get_sample_seg_ids() function")
  }
}

# prop cds/ads in training -------------------------------------------------
check_prop_train_cds <- function(d, train_segs_list, exp_prop_cds) {
  prop_cds_ads <- d %>% 
    distinct(seg_id, speech_register) %>% 
    filter(seg_id %in% train_segs_list) %>%
    group_by(speech_register) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(prop = n / sum(n))
  
  if( all(prop_cds_ads$prop == exp_prop_cds) ) {
    print(paste("test passed,", prop_cds_ads$prop[2], "of training data is CDS"))
  } else {
    print("test failed - check the get_sample_seg_ids() function")
  }
}

# no negative values in interpolated pitch contours ----------------------------

# no missing values in metadata columns ------------------------------------------
