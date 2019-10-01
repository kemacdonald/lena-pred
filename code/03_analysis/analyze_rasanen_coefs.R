source(here::here("code/00_config/lena-pred-libraries.R"))
source(here("code/00_config/lena-pred-config.R"))
dat_path <- "data/03_summaries/rasanen_2018/"

# Polynomial coefs --------------------------------------------------------

#d <- readMat(paste0(dat_path, "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"))
d <- readMat(paste0(dat_path, "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"))

# check okko's coefs
list_to_df <- function(list_item) {
  list_item[[1]] %>% as_tibble(.name_repair = NULL)
}

d_coefs <- d$F0PARAMS %>% map_df(list_to_df)

# plot the coefs and cluster assignments 
clusters <- kmeans(as.matrix(d_coefs), centers = 8)
d_coefs %>% mutate(cluster = clusters$cluster) -> d_coefs

ggplot(d_coefs, aes(V1, V2, color = as_factor(cluster))) + 
  geom_point()

# plot the distribution of clusters
d_coefs %>% count(cluster) %>% ggplot(aes(cluster, n)) + geom_bar(stat = 'identity')

# LSTM Training Data ------------------------------------------------------
d_lstm_train <- readMat(here(dat_path, "lstm_traindata.mat"))

# plot the distrubtion of training data clusters
d_lstm_train$train.in %>% as.vector() %>% qplot()
d_lstm_train$test.in %>% as.vector() %>% qplot()

# LSTM Preds --------------------------------------------------------------
d_lstm_preds <- readMat(here(dat_path, "preds_out.mat"))[["preds"]] %>% 
  as_tibble() %>% 
  janitor::clean_names() 

d_lstm_preds <- d_lstm_preds %>% 
  mutate(sample_id = 1:n()) %>% 
  gather(key = "shape_category", value = "prob_mass", -sample_id) %>% 
  group_by(sample_id) %>% 
  mutate(pred_cluster = which.max(prob_mass))

factor(d_lstm_preds$pred_cluster) %>% qplot()

# plot posterior prob dist
d_lstm_preds %>% 
  mutate(shape_category = str_extract(shape_category, "[:digit:]+") %>% as.numeric()) %>% 
  group_by(shape_category) %>% 
  summarise(m = mean(prob_mass)) %>% 
  ggplot(aes(x = as_factor(shape_category), y = m)) +
  geom_col()


# LSTM Posteriors comparison ---------------------------------------------------------

# From Okko: Here're LSTM posteriors for Q = 12 codebook quantization in Matlab tensor DATA 
# (1070 x 345 x 12, i.e., utterance x frame (100ms frames) x VQ index. "filenames" is a Matlab 
# cell array that contains 1070 filenames corresponding to the DATA rows.

# convert 3d array to dataframe to compare with our lstm output
d_lstm_ras <- readMat(here(dat_path, "LSTM_posteriors_to_Kyle_CB12_1.mat"))
ras_filenames <- d_lstm_ras$filenames %>% unlist() %>% str_remove(".wav") %>% str_extract(pattern = "(?<=Label\\/).*")
d_ras_data <- d_lstm_ras[["DATA"]] 

# loop over array rows (files) and build dataframe of posteriors for each timeframe
d_ras_final <- list()

for (i in seq_along(1:length(ras_filenames))) {
  one_file <- as_tibble(d_ras_data[i,,]) %>% 
    mutate(time_bin_id = 1:nrow(.),
           row_sum = rowSums(.)) %>% 
    filter(row_sum > 0)  %>%
    select(-row_sum) %>% 
    gather(key = "cluster_shape", value = "prob_mass", -time_bin_id) %>%
    mutate(seg_id = ras_filenames[i],
           cluster_shape = str_replace(cluster_shape, "V", "shape_"))
  
  d_ras_final[[i]] <- one_file
}

d_ras_final <- d_ras_final %>% bind_rows()

# lstm  results
preds_files <- list.files(config_obj$paths_config$lstm_preds_path)

runs <- c("run1", "run2")

d <- preds_files %>% 
  map(read_lena_pred_data,
      config_obj, 
      f_type = "rds", 
      is_pitch = FALSE) %>%
  setNames(runs) 
  #setNames(config_obj$exp_config$runs) 

d <- d %>% future_map2_dfr(.f = process_exp_run, .y = names(.))
d$speech_register <- factor(d$speech_register) %>% fct_rev()
d_lstm_mac <- d %>% filter(exp_run_id == "run1", fold_id == "fold1", n_qshapes == 12)

# get the same segids in ras and mac and compare posteriors

overlapping_segids <- intersect(unique(d_lstm_mac$seg_id),
                            unique(d_ras_final$seg_id))

d_ras_filt <- d_ras_final %>% 
  filter(seg_id %in% overlapping_segids) %>% 
  select(seg_id, prob_mass, time_bin_id, cluster_shape) %>% 
  mutate(implementation = "rasanen",
         time_bin_id = as.character(time_bin_id))

d_compare <- d_lstm_mac %>% 
  select(seg_id, prob_mass, time_bin_id, cluster_shape) %>% 
  mutate(implementation = "macdonald") %>% 
  bind_rows(d_ras_filt)

# plot posteriors for each implementation (what to do about all zeros in okko's data?)

ms_compare <- d_compare %>% 
  group_by(implementation, seg_id, cluster_shape) %>% 
  summarise(m_seg = mean(prob_mass)) %>% 
  group_by(implementation, cluster_shape) %>% 
  summarise(m = mean(m_seg))

ms_compare %>% 
  mutate(shape_num = str_extract(cluster_shape, "[:digit:]")) %>% 
  ggplot(aes(x = shape_num, y = m)) +
  geom_bar(stat = "identity") +
  facet_wrap(~implementation)
