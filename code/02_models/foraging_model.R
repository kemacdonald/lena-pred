library(tidyverse)
theme_set(brms::theme_black())


# build functions

# estimate skill at one age point
estimate_know <- function(age, k) {
  1 - exp(-k*age)
}

# age-related decline 
estimate_decline <- function(age, m) {
  exp(-m * age)
}

# okay now build the full age-specific skill model

estimate_skill <- function(age, growth_rate, decline_rate, know_elasticity) {
  estimate_decline(age, decline_rate) * estimate_know(age, growth_rate)^know_elasticity
}

sim_skill_dev <- function(max_age, growth_rate, decline_rate, know_elasticity) {
  age_range <- seq(0, max_age, by = 1)
  tibble(
    growth_rate = growth_rate,
    decline_rate = decline_rate,
    estimate = age_range %>% purrr::map_dbl(estimate_skill,
                                                  growth_rate,
                                                  decline_rate,
                                                  know_elasticity),
    know_elasticity = know_elasticity,
    age = age_range,
    sim_id = 1
  )
}


sim_results_skill <- sim_skill_dev(max_age = 80,
                                   growth_rate = 0.01, 
                                   decline_rate = 0.04,
                                   know_elasticity = 10)

sim_results_skill %>% 
  ggplot(aes(x = age, y = estimate,
             group = sim_id)) +
  geom_line(color = "white") 


# simulate knowledge growth at different values of k parameter
sim_know_growth <- function(k, max_age) {
  age_range <- seq(0, max_age, by = 1)
  tibble(
    k_value = k,
    estimate = age_range %>% purrr::map_dbl(estimate_know, k = k),
    age = age_range
  )
}



sim_know_decline <- function(m, max_age) {
  age_range <- seq(0, max_age, by = 1)
  tibble(
    m_value = m,
    estimate = age_range %>% purrr::map_dbl(estimate_decline, m),
    age = age_range
  )
}

#### simulate knowledge increase
rate_param <- seq(0.05, 0.3 , by = 0.025)

sim_results_know <- rate_param %>% 
  purrr::map_dfr(sim_know_growth, max_age = 80) 
  
a <- sim_results_know %>% 
  ggplot(aes(x = age, y = estimate,
             group = k_value)) +
  geom_line(color = "white") +
  ggtitle('knowledge growth')


sim_results_decline <- rate_param %>% 
  purrr::map_dfr(sim_know_decline, max_age = 80) 

b <- sim_results_decline %>% 
  ggplot(aes(x = age, y = estimate,
             group = m_value)) +
  geom_line(color = "white") +
  ggtitle('cog decline')

cowplot::plot_grid(a, b)