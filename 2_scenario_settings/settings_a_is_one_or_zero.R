# ------------------------------------------------------------------------------------------
# Settings:
# ------------------------------------------------------------------------------------------

# Set user items dimentions and number of items to answer
source("1_thetas_betas/parameter_settings_thetas_betas.R")

# A is 1 or .1 with percentage 80% and 20%
A = sample(c(1,.1), Ni, replace = TRUE, prob = c(.8,.2) ) # simulated discrimination parameter
