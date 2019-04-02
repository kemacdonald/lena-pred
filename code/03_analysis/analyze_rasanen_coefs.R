library(here)
library(R.matlab)
library(tidyverse)
dat_path <- "data/03_summaries/rasanen_2018"

## polynomial coefs

d <- readMat(here(path, "results_19-Oct-2017 21_07_23_ManyBabies_usesyllables0_framesize_100.mat"))

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


## LSTM training data structure

d_lstm_ras <- readMat(here(dat_path, "lstm_traindata.mat"))
