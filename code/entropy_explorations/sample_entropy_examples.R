library(pracma)
library(tidyverse)

# zero entropy
ts <- rep(61:65, 40)
zero_ent <- qplot(seq(1, length(ts)), ts, geom = 'line') + 
  labs(x = "X", y = "Y", subtitle  = paste('sample entropy = ', 
                                       sample_entropy(ts)))

# highly deterministic time series (sine wave)
low_det <- qplot(seq(1,100,by=0.2), sin(seq(1,100,by=0.2)), geom = 'line') +
  labs(x = "X", y = "Y", 
       subtitle  = paste('sample entropy = ', 
                         sample_entropy(sin(seq(1,100, by=0.2))) %>% round(2)))

# random noise
high_rand <- qplot(seq(1, 500), rnorm(500), geom = 'line') +
  labs(x = "X", y = "Y", 
       subtitle  = paste('sample entropy = ', 
                         sample_entropy(rnorm(500),) %>% round(2)))

#### Why isn't entropy 0 for sine wave?
d_sin <-  sin(seq(1,100, by=0.2)) 
r_vals <-  seq(0.1, 4, by = 0.25) * sd(d_sin)
samp_ents <- r_vals %>% purrr::map_dbl(function(x) sample_entropy(ts = d_sin, r = x)) 

rsim_sin <- qplot(r_vals, samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(x = c(0, 4), y = c(0,3.2))

# vary the embedding dimension
edim_vals <- 2:25
samp_ents_m <- edim_vals %>% purrr::map_dbl(function(x) sample_entropy(ts = d_sin, edim = x)) 

qplot(edim_vals, samp_ents_m, geom = c('line', 'point')) + 
  lims(y = c(0, 0.4)) +
  labs(x = 'size of embedding', y = 'sample entropy')

## What about the repeating pattern? 
ts_r_vals <- seq(0.1, 4, by = 0.25) * sd(ts)
ts_samp_ents <- ts_r_vals %>% purrr::map_dbl(function(x) sample_entropy(ts = ts, r = x)) 

rsim_ts <- qplot(ts_r_vals, ts_samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(x = c(0, 4), y = c(0, 3.2))

## What about random noise? 
d_rand <- rnorm(200)
rand_r_vals <- seq(0.1, 4, by = 0.25) * sd(d_rand)
rand_samp_ents <- rand_r_vals %>% purrr::map_dbl(function(x) sample_entropy(ts = d_rand, r = x)) 

rsim_rand <- qplot(rand_r_vals, rand_samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(x = c(0, 4), y = c(0,3.2))

## plot timeseries with entropy s a function of tolerance

cowplot::plot_grid(zero_ent, rsim_ts,
                   low_det, rsim_sin,
                   high_rand, rsim_rand,
                   scale = 0.8,
                   ncol = 2)

## What about the number of samples within a time frame?

sample_entropy(sin(seq(1,100, by=0.2))) # ~500 data points
sample_entropy(sin(seq(1,100, by=0.1))) # ~1000 data points

## What about the length of time series

sample_entropy(sin(seq(1,10, by=0.2)), edim = 1) # ~500 data points
sample_entropy(sin(seq(1,100, by=0.1))) # ~1000 data points
