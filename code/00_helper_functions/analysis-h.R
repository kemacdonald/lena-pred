## Analysis helpers ---------------------------------------------------

# read data files for analysis
read_lena_pred_data <- function(file_prefix = NULL, config_object = NULL, f_type = NULL, is_pitch = NULL) {
  
  file_name <- paste(file_prefix, 
                     config_object$exp_config$dataset_name,
                     config_object$kmeans_config$scale_coefs,
                     sep = "-")
  
  if (f_type == "rds") {
    
    if (is_pitch) {
      read_rds(here(config_obj$paths_config$pitch_sum_path, paste0(file_name, ".rds")))
    } else {
      d <- read_rds(here(config_object$paths_config$lstm_sum_path, 
                         paste0(file_name,  ".rds"))) %>% 
        flatten() %>% 
        map2_df(.y = names(.), .f = extract_preds)
      
      d$speech_register <- factor(d$speech_register) %>% fct_rev() # reverses order of factor labels for plotting
      d  
    }
    
    
  } else if (f_type == "csv") {
    read_csv(here(config_object$paths_config$lstm_sum_path, paste0(file_name,  ".csv")))
  }
}

# extract tidy preds data frame from results object
extract_preds <- function(d_obj, model_name) {
  p_cs <- get_prop_cds(model_name)
  nq <- get_nqshapes(model_name)
  
  pluck(d_obj, "result", "d_preds") %>% 
    mutate(prop_cds_train = p_cs,
           n_qshapes = nq)
}

# analyze one run of experiment
# takes data frame with preds
# returns average predictability of CDS vs. ADS with 95% bootstrapped CIs
analyze_one_exp <- function(d = NULL, metric = NULL) { 
  d %>% 
    group_by(seg_id, speaker_id, speech_register) %>% 
    summarise(m_seg = mean(prob_mass), m_acc_seg = mean(correct_pred)) %>% 
    group_by(speaker_id, speech_register) %>% 
    summarise(m_prob = mean(m_seg), m_acc = mean(m_acc_seg)) %>% 
    group_by(speech_register) %>% 
    tidyboot_mean(column = {{ metric }}) 
}

get_nqshapes <- function(s) {
  str_split(s, "_", simplify = T)[2] %>% str_extract(pattern = "[:digit:]+") %>% as.numeric()
}

get_prop_cds <- function(s) {
  str_split(s, "_", simplify = T)[1] %>% str_extract(pattern = "(?<=cds).*$")
}

# TODO: fix perplexity function
compute_perplexity <- function(d) {
  cross_entropy <- keras::loss_categorical_crossentropy(y_true, y_pred)
  2^cross_entropy
}

# TODO: fix this function  to handle duration model
get_bda_results <- function(m_fit, var_name) {
  post <- posterior_samples(m_fit)
  
  m_summary <- post %>% 
    mutate(p_ADS = inv_logit_scaled(b_Intercept),
           p_IDS = inv_logit_scaled(b_Intercept + b_speech_registerIDS),
           diff_correct = p_ADS - p_IDS,
           sample_id = 1:nrow(.)) %>% 
    select(sample_id, p_ADS, p_IDS, diff_correct) %>% 
    gather(key = "type", value = "value", -sample_id) %>% 
    group_by(type) %>% 
    median_hdi(.width = 0.95)
  
  model_results <- list(post_samples = post, summary = m_summary)
}

## Baseline models --------------------------------------------

score_persistence_alg <- function(d, model_name) {
  d_preds <- tibble(sample_id = 1:length(d$train_data$next_cluster),
                    tminus_1 = lag(d$train_data$next_cluster) %>% as.integer(), 
                    tplus_1 = lead(d$train_data$next_cluster) %>% as.integer())
  
  # remove first row since you there's no prior data to predict next value
  d_preds <- d_preds %>% filter(!is.na(tminus_1)) 
  
  # score accuracy (how often does t-1 correctly predict t+1?)
  d_preds <- d_preds %>% mutate(correct_pred = ifelse(tminus_1 == tplus_1, 1, 0))
  
  acc <- round(mean(d_preds$correct_pred, na.rm = T), 2)
  
  # if ( str_detect(d$actual_prop_cds_train$speech_register, "ADS") %>% all() ) {
  #   prop_cds <- 1 - d$actual_prop_cds_train %>% filter(speech_register == "ADS") %>% pull(prop) 
  # } else {
  #   prop_cds <- d$actual_prop_cds_train %>% filter(speech_register == "IDS") %>% pull(prop) 
  # }
  
  p_cs <- get_prop_cds(model_name)
  nq <- get_nqshapes(model_name)
  
  tibble(
    prop_cds = p_cs,
    n_qshapes = nq,
    acc = acc
  )
}
