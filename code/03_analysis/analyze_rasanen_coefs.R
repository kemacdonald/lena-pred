source(here::here("code/00_config/lena-pred-libraries.R"))
dat_path <- "data/03_summaries/rasanen_2018"


# Polynomial coefs --------------------------------------------------------
d <- readMat(here(dat_path, "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"))

qplot(seq(1, length(blah)), blah, geom = 'line')

# check okko's coefs
list_to_df <- function(list_item) {
  list_item[[1]] %>% as_tibble(.name_repair = NULL)
}

d_coefs <- d$F0PARAMS %>% map_df(list_to_df)

# plot the coefs and cluster assignments 
clusters <- kmeans(as.matrix(d_coefs), centers = 8 )
d_coefs %>% mutate(cluster = clusters$cluster) -> d_coefs

ggplot(d_coefs, aes(V1, V2, color = as_factor(cluster))) + 
  geom_point()

# plot the distribution of clusters
d_coefs %>% count(cluster) %>% ggplot(aes(cluster, n)) + geom_bar(stat = 'identity')

# LSTM Training Data ------------------------------------------------------
d_lstm_train <- readMat(here(dat_path, "lstm_traindata.mat"))


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