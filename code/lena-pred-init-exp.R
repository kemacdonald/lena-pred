# Experiment Init Script ---------------------------------------------------------

source(here::here("code/00_config/lena-pred-libraries.R")) 
source(here("code/00_config/lena-pred-config.R"))    
plan(multiprocess)

# for each fold of test data, we now generate a dataset,
# train model, and generate predictions
d_results <- vector(mode="list", length=length(1:length(config_obj$exp_config$runs)))

# this will return a list of dnn predictions for 
# each run of the experiment with k folds
# after each run, the results will be saved automatically in experimental_runs/ 
# you can specify if you want to run the pitch to poly coefs pipeline 
# and/or the poly coefss to clusters for reach run of the experiment 

config_obj$exp_config$runs %>% 
  map(run_experiment, 
      config_object = config_obj, 
      k = config_obj$exp_config$n_folds,
      run_pitch_to_coefs = FALSE,
      run_coefs_to_clusters = TRUE)

print("Completed and saved all runs of the experiment!")