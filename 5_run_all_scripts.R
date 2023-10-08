# -------------------------------------------
# Run all scrips from start to finish
# -------------------------------------------

library('rmarkdown')

# Simulate for scenario neg
source("a-parameter_negative_a/1_simulate_data/Simulate_neg.R")
source("a-parameter_negative_a/3_ltm_2pl/parallel_LTM_2PL_estimations.R")
source("a-parameter_negative_a/4_glm/parallel_glm_estimations.R")
source("a-parameter_negative_a/4_glm_2par/parallel_glm_2par_estimations.R")
source("a-parameter_negative_a/5_newton_raphson/parallel_NR_estimations.R")
source("a-parameter_negative_a/7_merge_estimations/merge_estimations.R")
rmarkdown::render("a-parameter_negative_a/8_analysis_bias_sem/bias_mse_neg.Rmd")
rmarkdown::render("a-parameter_negative_a/9_analyse_sensitivity_specificity/sensitivity_specificity.Rmd")

# Simulate for scenario one
source("a-parameter_one_at_75/1_simulate_data/Simulate_one.R")
source("a-parameter_one_at_75/3_ltm_2pl/parallel_LTM_2PL_estimations.R")
source("a-parameter_one_at_75/4_glm/parallel_glm_estimations.R")
source("a-parameter_one_at_75/4_glm_2par/parallel_glm_2par_estimations.R")
source("a-parameter_one_at_75/5_newton_raphson/parallel_NR_estimations.R")
source("a-parameter_one_at_75/7_merge_estimations/merge_estimations.R")
rmarkdown::render("a-parameter_one_at_75/8_analysis_bias_sem/bias_mse_neg.Rmd")

# Simulate for scenario neg
source("a-parameter_zero_or_one/1_simulate_data/Simulate_zero_or_one.R")
source("a-parameter_zero_or_one/3_ltm_2pl/parallel_LTM_2PL_estimations.R")
source("a-parameter_zero_or_one/4_glm/parallel_glm_estimations.R")
source("a-parameter_zero_or_one/4_glm_2par/parallel_glm_2par_estimations.R")
source("a-parameter_zero_or_one/5_newton_raphson/parallel_NR_estimations.R")
source("a-parameter_zero_or_one/7_merge_estimations/merge_estimations.R")
rmarkdown::render("a-parameter_zero_or_one/8_analysis_bias_sem/bias_mse_neg.Rmd")
rmarkdown::render("a-parameter_zero_or_one/9_analyse_sensitivity_specificity/sensitivity_specificity.Rmd")

# Compare all bias and mse
rmarkdown::render("compare_all_bias_mse/compare_all_bias_mse.Rmd")

# Compare all sensitivity and specificity
rmarkdown::render("compare_all_sensitivity_specificity/compare_all_sensitivity_specificity.Rmd")

# Create exploration plots
source("exploration/recovery_plots.R")
