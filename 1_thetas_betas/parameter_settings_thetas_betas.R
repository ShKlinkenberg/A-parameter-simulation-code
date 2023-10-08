# ------------------------------------------------------------------------------------------
# Settings:
# ------------------------------------------------------------------------------------------

# Set seed
source("0_seed.R")

# Set user items dimentions and number of items to answer
Np = 6000
Ni = 600
N  = Np * 500

# -------------
# Debug mode
# ------------

# # Set user items dimentions and number of items to answer
# Np = 500
# Ni = 500
# N  = Np * 500

# Simulate theta's and beta's
T = rnorm(Np,  0.0, 1.0) # real theta
B = runif(Ni, -3.0, 3.0) # real betas
