# ------------------------------------------------------------------------------------------
# LTM parameter estimation
# ------------------------------------------------------------------------------------------

ltmEstimations <- function(data_wide, method, scenario, folder) {

# Load libraries
library("ltm")     # install.packages("ltm")
library("stringr") # install.packages("stringr")

# Fit 2 PL for selected data
# data_wide is passed through scenario folder

# Set minimal amount of data required
UsersToUse <- apply(!is.na(data_wide), 1, sum) > 20 # Users are at least answered by X items
ItmesToUse <- apply(!is.na(data_wide), 2, sum) > 50 # Items are at least answered by Y users

# Transform to data frame
data_wide <- as.data.frame(data_wide)
# Make item names explicit and numeric form factor
names(data_wide) <- as.numeric(as.character(str_extract(names(data_wide), "[0-9]+")))

data_wide <- data_wide[UsersToUse,ItmesToUse]
# data_wide

# Fit ltm to constrained data
Sys.time()
fit2pl <- ltm(data_wide ~ z1, na.action = NULL)
Sys.time()

# Make item names explicit
item_id <- as.numeric(row.names(coef(fit2pl)) )

ltm..beta <- coef(fit2pl)[,"Dffclt"]
ltm..a    <- coef(fit2pl)[,"Dscrmn"]

# Create dataframe with estimations
ltm..estimations <- data.frame(item_id, ltm..beta, ltm..a)

# Plot a coeficients
# plot(fit2pl, zrange = c(-55, 55), ylim = c(-0.1, 1))

# coef(fit2pl)

# ------------------------------------------------------------------------------------------
# Extract thetas
# ------------------------------------------------------------------------------------------

theta.estimations <- factor.scores(fit2pl, data_wide)$score.dat[,c('z1', 'se.z1')]
# Set user id's
theta.estimations$user_id <- row.names(theta.estimations)
# Reorder columns
theta.estimations <- theta.estimations[, c(3,1,2)]

# ------------------------------------------------------------------------------------------
# Start correcting for flipped signs in estimations
# ------------------------------------------------------------------------------------------

# Import simulation settings
file.settings <- list.files("2_scenario_settings", pattern = paste0(scenario, "\\.R$"), full.names = T )
source(file.settings)

real.beta.a <- data.frame(item_id = as.numeric(1:Ni), B, A)
real.theta  <- data.frame(user_id = as.numeric(1:Np), T)

beta.merged  <- merge(real.beta.a, ltm..estimations,  by = "item_id", all.x = TRUE)
theta.merged <- merge(real.theta,  theta.estimations, by = "user_id", all.x = TRUE)

## TEST new flip method
flip.B <- sign(sum(sign(beta.merged$B  *  beta.merged[, "ltm..beta"]), na.rm=T))
flip.A <- sign(sum(sign(beta.merged$A  *  beta.merged[, "ltm..a"]),    na.rm=T))
flip.T <- sign(sum(sign(theta.merged$T * theta.merged[, "z1"]),        na.rm=T))

# Flip estimations
ltm..estimations[, "ltm..beta"] <- flip.B * ltm..estimations[, "ltm..beta"]
ltm..estimations[, "ltm..a"]    <- flip.A * ltm..estimations[, "ltm..a"]
theta.estimations[, "z1"]       <- flip.T * theta.estimations[, "z1"]

# ------------------------------------------------------------------------------------------
# Replace generic names with scenario and method names
# ------------------------------------------------------------------------------------------

# Replace generic column name with method name
names(theta.estimations) <- gsub("z1",paste0("theta.",method,".",scenario), names(theta.estimations))

# Replace generic column names with method name
names(ltm..estimations) <- gsub("\\.\\.", paste0("\\.",method,".",scenario,"\\."), names(ltm..estimations))

# Replace general object name with method object name
ltm.name   <- paste0("ltm.",method,".estimations");             assign(ltm.name, ltm..estimations) 
model.name <- paste0("fit2pl.",method);                         assign(model.name, fit2pl)
theta.name <- paste0("theta.estimation.",method,".", scenario); assign(theta.name, theta.estimations)

# Save method objects to method file
save(list = c(ltm.name, model.name, theta.name),
     file = paste0(folder,"3_ltm_2pl/ltm_",method,"_estimations.Rdata") )

# load(file = paste0(folder,"3_ltm_2pl/ltm_",method,"_estimations.Rdata") )

} 