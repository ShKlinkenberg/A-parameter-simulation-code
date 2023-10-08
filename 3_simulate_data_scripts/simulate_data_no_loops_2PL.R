# Import simulation functions 
source("0_functions/sim_functions.R")

# Import large file storage function
source("0_functions/LFS_function.R")

# Set user items dimentions and nomber of items to answer
number_of_users = Np
number_of_items = Ni
# number_of_items_to_answer = 20

# Create user and item vectors
user_id = 1:number_of_users
item_id = 1:number_of_items

# Sample random normal user and item ratings
user_domain_rating = T
item_rating        = B 
# Sample item discrimination parameters
item_a_par         = A

# Setup user and item data frames
users <- data.frame(user_id, user_domain_rating)
items <- data.frame(item_id, item_rating, item_a_par)

# # Save item table for later use
# folder is created in scenario
save(items, file = paste0(folder, "2_simulated_data/simulated_a_par.Rdata") )

# ------------------------------------------------------------------------------------------
# Start simulation 
# ------------------------------------------------------------------------------------------

# Create data frame with all permutations on user and item numbers
users_by_items <- expand.grid(users$user_id, items$item_id)

# Change names to match tables from SQL database
names(users_by_items) <- c('user_id', 'item_id')

# Left join all user item combinations with user and item tables. To add all abilities, 
# difficulties and a-parameters to each row.
users_by_items <- merge(users_by_items, users, by = "user_id", all.x = T)
users_by_items <- merge(users_by_items, items, by = "item_id", all.x = T)
# Getting warnings but all seems to work

# Calculate probability for a correct answer based on ability, difficulty and a-parameter.
# First: discrimination times theta minus beta
users_by_items$atb <- users_by_items$item_a_par * (users_by_items$user_domain_rating - users_by_items$item_rating)
# Then: apply logistic fuction to calculate probability
users_by_items$p <- 1 / ( 1 + exp(-users_by_items$atb) )

# Sample binary responses from binomial distribution.
users_by_items$correct_answered <- rbinom(length(users_by_items$p), 1, users_by_items$p)
# users_by_items$correct_answered <- 1*(users_by_items$p >= runif(nrow(users_by_items),0,1))

# Create subset from all responses, based on the math garden item selection algorithm where items
# are sampled to which the user has a mean probability of answering correct of .75 with sigma of .1

# Set the criteria by transforming the probabilities on answering correct to a probability to 
# get selected. Here the quantile input is p which is transformed through a normal probability 
# density function to output the density value. The density is then scaled to be between 0 and 1.
users_by_items$p_selection <- pSelection(users_by_items$p, target_mu = .75, target_sigma = .1)

# Finaly we make a random binomial selection, based on the probability for selection. 
users_by_items$selected <- rbinom(length(users_by_items$p_selection), 1, users_by_items$p_selection)
#users_by_items$selected <- 1*(users_by_items$p_selection >= runif(nrow(users_by_items),0,1))

# Create final datasets (long & wide):

users_by_items <- users_by_items[order(users_by_items$item_id, users_by_items$user_id),]

  full_data_long <- users_by_items
sparse_data_long <- users_by_items[users_by_items$selected == 1,]

full_data_wide <- matrix(data = users_by_items$correct_answered, 
                         nrow = number_of_users, 
                         ncol = number_of_items, byrow = FALSE)

index <- matrix(data = users_by_items$selected, 
                nrow = number_of_users, 
                ncol = number_of_items, byrow = FALSE)

sparse_data_wide <- full_data_wide
sparse_data_wide[index == 0] <- NA

# Sanity check of simulation
plot(items$item_rating, apply(full_data_wide, 2, mean),              xlab = "beta", ylab = "Proportion correct", main = "PropCor for full")
plot(items$item_rating, apply(sparse_data_wide,  2, mean, na.rm=T),  xlab = "beta", ylab = "Proportion correct", main = "PropCor for sparse")

# ------------------------------------------------------------------------------------------
# Save results 
# ------------------------------------------------------------------------------------------

save(full_data_long,   file = paste0(folder, "2_simulated_data/full_data_long.Rdata")   ) # Long form complete dataset
save(sparse_data_long, file = paste0(folder, "2_simulated_data/sparse_data_long.Rdata") ) # Long form subset with probability correct of .75
save(full_data_wide,   file = paste0(folder, "2_simulated_data/full_data_wide.Rdata")   ) # Wide form complete dataset
save(sparse_data_wide, file = paste0(folder, "2_simulated_data/sparse_data_wide.Rdata") ) # Wide form subset with probability correct of .75

# Commit and Push to Github using GitHub Large File Storage
# GitCommitPushLFS("2_simulated_data/full_data_long.rdata")
# GitCommitPushLFS("2_simulated_data/sparse_data_long.rdata")
# GitCommitPushLFS("2_simulated_data/full_data_wide.rdata")
# GitCommitPushLFS("2_simulated_data/sparse_data_wide.rdata")
