library(TSEntropies)

# vary the tolerance parameter for a deterministic time series
ts <- rep(61:65, 10)
ts_r_vals <- seq(0.1, 2, by = 0.1) * sd(ts)
ts_samp_ents <- ts_r_vals %>% purrr::map_dbl(function(x) SampEn(ts, r = x)) 

rsim_ts <- qplot(ts_r_vals, ts_samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(y = c(0, 0.5))


# vary the tolerance parameter for a sine wave
d_sin <-  sin(seq(1,100, by=0.2)) 
r_vals <-  seq(0.1, 2, by = 0.1) * sd(d_sin)
samp_ents <- r_vals %>% purrr::map_dbl(function(x) SampEn(d_sin, r = x)) 

rsim_sin <- qplot(r_vals, samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(x = c(0, 2), y = c(0, 0.5))


#########

# vary the tolerance parameter for a periodic time series

ts <- rep(61:65, 10)
ts_r_vals <- seq(0.1, 2, by = 0.1) * sd(ts)
ts_samp_ents <- ts_r_vals %>% purrr::map_dbl(function(x) sample_entropy(ts, r = x)) 

rsim_ts <- qplot(ts_r_vals, ts_samp_ents, geom = c('line', 'point')) + 
  labs(x = 'tolerance value', y = 'sample entropy') +
  lims(y = c(0, 0.5))

## vary the precision of the measurements in a time series 
## (should increase entropy of continous variable)
d_sin <- sin(sin(seq(1,100, by=0.05)))
d_sin1 <- sin(sin(seq(1,100, by=0.1)))
d_sin2 <- sin(sin(seq(1,100, by=1)))
d_sin3 <- sin(sin(seq(1,100, by=2)))
d_sin4 <- sin(sin(seq(1,100, by=3)))

qplot(seq(1,100, by=0.1), d_sin1, geom = c('point', 'line'))
qplot(seq(1,100, by=1), d_sin2, geom = c('point', 'line'))
qplot(seq(1,100, by=2), d_sin3, geom = c('point', 'line'))
qplot(seq(1,100, by=3), d_sin4, geom = c('point', 'line'))

SampEn(d_sin)
SampEn(d_sin1)
SampEn(d_sin2)
SampEn(d_sin3)
SampEn(d_sin4)

