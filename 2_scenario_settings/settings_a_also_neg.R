# ------------------------------------------------------------------------------------------
# Settings:
# ------------------------------------------------------------------------------------------

# Set user items dimentions and number of items to answer
source("1_thetas_betas/parameter_settings_thetas_betas.R")

# Set a-parameter between .1 and 2.5
A = runif(Ni, 0.1, 2.5)      # gesimuleerde discriminatie parameters
