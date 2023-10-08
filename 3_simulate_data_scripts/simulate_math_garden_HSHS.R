# ------------------------------------------------------------------------------------------
# Simulate Math Garden - Klinkenberg                 
# ------------------------------------------------------------------------------------------

# Clear memory
# rm(list = ls())

# Load packages
# install.packages('readr')
# library('readr')

# Source CAT functions
source("0_functions/cat_functions.R")

# Source large file upload
source("0_functions/LFS_function.R")

# ------------------------------------------------------------------------------------------
# # Select and load simulated data based on three scenarios
# settings <- 'neg'
# 
# if(settings == 'one') source('1_simulate_data/settings_a_is_one.R')   # All a-parameters are one
# if(settings == 'uni') source('1_simulate_data/settings_uniform.R')    # A-parameters are uniform between 0:1
# if(settings == 'neg') source('1_simulate_data/settings_a_also_neg.R') # A-parameters are uniform between -.5:2

# Imported variables
#
# Vars: 
#   A  : Vector with simulated a-parameters      
#   B  : Vector with simulated beta's
#   N  : Total number of iterations (users times number of items per user)
#   Ni : Number of items
#   Np : Number of users
#   T  : Vector with theta's

# ------------------------------------------------------------------------------------------
# Math garden Elo settings
# ------------------------------------------------------------------------------------------

# Set difficulty distribution parameters.
mu.difficult = .6;  sigma.difficult = .05 # Settings for item selection: Difficult
mu.medium    = .75; sigma.medium    = .1 	# Settings for item selection: Medium
mu.easy      = .9;  sigma.easy      = .05 # Settings for item selection: Easy

# Set playing difficutly levels
pkeuze <- rep(2, Np) # of: pkeuze = sample(1:3, Np, replace=TRUE)

# Set target probabilities correct and standard deviation for different difficulty levels
mu = sigma = rep(0,Np)
mu[pkeuze==1] = mu.difficult; sigma[pkeuze==1] = sigma.difficult
mu[pkeuze==2] = mu.medium;    sigma[pkeuze==2] = sigma.medium
mu[pkeuze==3] = mu.easy;      sigma[pkeuze==3] = sigma.easy

# quantilesetting = .05		         # New subjects enter at quantile value equal to quantilesetting
                                   # Give a start rating corresponding to quentilesetting

# Set number of items per user
number.of.items.per.user = 15

# Set response deadline
deadline = 20

# ------------------------------------------------------------------------------------------
# Setup excluded items ring. Use this matrix to track last administered items that are 
# to be excluded form item selection
# ------------------------------------------------------------------------------------------

# Number of previous items to be excluded from resampling
number.of.items.to.be.excluded = 20

# Set initial matrix to track items to be excluded for all players
excluded.item.matrix <- matrix(NA, Np, number.of.items.to.be.excluded) 

# ------------------------------------------------------------------------------------------
# Create Item and User tables an log records file
# ------------------------------------------------------------------------------------------

# Create Item table
items <- data.frame(item_id = 1:Ni)
# Set initial values
items$rating.estimated         <- 0 # B # Set start rating here
items$rating.simulated         <- B
items$discrimination.estimated <- 1 # A # Set start parameter here
items$discrimination.simulated <- A
items$modified_count           <- 0
items$rating_uncertainty       <- 0
items$k_factor                 <- 0

# Create User table
users <- data.frame(user_id = 1:Np)
# Set initial values
users$rating.estimated   <- 0 # T # Set start rating here
users$rating.simulated   <- T
users$modified_count     <- 0
users$rating_uncertainty <- 0
users$k_factor           <- 0

# Create response log file names
variable.names <- c("trial", 
                    "user_id", 
                    "item_id",
                    "user_domain_rating", 
                    "true_user_domain_rating",
                    "item_rating", 
                    "true_item_rating", 
                    "true_discrimination_item",
                    "correct_answered", 
                    "response_time",
                    "score", 
                    "expected_score",
                    "RU_player", 
                    "RU_item",
                    "k_player", 
                    "k_item",
                    "new_RU_player",
                    "new_RU_item",
                    "new_k_player", 
                    "new_k_item",
                    "new_elo_player",
                    "new_elo_item",
                    "paired_item_id",
                    "paired_item_rating") 

# Create log records file with variable names
log.records <- file(paste0(folder,"2_simulated_data/log_records.csv"))
# Write file names to file
cat(variable.names, "paired\n", file = log.records)
# Close log records file
close(log.records)

# Create connection to log records file for appending (open = "a") results
log.records <- file(paste0(folder,"2_simulated_data/log_records.csv"), open = "a")

# ------------------------------------------------------------------------------------------
# Prepare main loop
# ------------------------------------------------------------------------------------------

# Set initial variable values to zero
selected.user <- previous.user <- previous.expected.score <- previous.score <- 0

# Debug mode
# set.seed(1) 
# N = 600
# trial=2; previous.item=1 # Debug mode

# ------------------------------------------------------------------------------------------
# Main loop
# ------------------------------------------------------------------------------------------

for (trial in 1:N) {
  
  # Sample user on trial 1 and after every 15 trials. 
  # This is the number of items per subject before new subject.
  if(trial%%number.of.items.per.user==0 | trial==1) {
    selected.user <- sample(users[, 'user_id'], 1)
  }  
  
  # Sample random probability with initialized mu and sigma.
  target.probability <- TargetProbability(mu[selected.user], sigma[selected.user])
  
  # Calculate target betas based on desired probability
  target.betas <- TargetProbabilityToBetas(TargetProbability = target.probability, 
                                           discriminations   = items$discrimination.estimated, # Use current estimate of a-parameters
                                           theta             = users[selected.user, "rating.estimated"])
  
  # Sample item from subset of allowed items
  all.items        <- items$item_id                         # Select all items
  items.to.exclude <- excluded.item.matrix[selected.user, ] # Select all items to exclude
  selected.item    <- SelectItemFromTargetBetas(target.betas         = target.betas,
                                                all.estimated.betas  = items$rating.estimated,
                                                all.item.frequencies = items$modified_count,
                                                items.to.exclude     = items.to.exclude)
  
  # Add selected item to exclusion list for next trail
  excluded.item.matrix[selected.user, ] <- ExcludedItemRing(items.to.exclude, selected.item)

  # Update user and item count
  users[selected.user, "modified_count"] <- users[selected.user, "modified_count"] + 1  
  items[selected.item, "modified_count"] <- items[selected.item, "modified_count"] + 1

  # Log records
  cat(trial, 
      selected.user, 
      selected.item, 
      users[selected.user, "rating.estimated"],
      users[selected.user, "rating.simulated"],
      items[selected.item, "rating.estimated"],
      items[selected.item, "rating.simulated"],
      items[selected.item, "discrimination.simulated"],
      "",
      file=log.records)
  
  # ------------------------------------------------------------------------------------------
  # Sim: Response & Response Time, Score & Expected Score
  # ------------------------------------------------------------------------------------------
  
  # Sample response from binomial distribution
  correct <- SampleResponse(theta       = users[selected.user, "rating.simulated"],
                            beta        = items[selected.item, "rating.simulated"],
                            a.parameter = items[selected.item, "discrimination.simulated"]
                           )
  
  # Sample response time when response is:
  time <- SampleResponseTime(correct     = correct,
                             theta       = users[selected.user, "rating.simulated"],
                             beta        = items[selected.item, "rating.simulated"],
                             a.parameter = items[selected.item, "discrimination.simulated"],
                             deadline    = deadline
                            )
  
  # Calculate expected score
  expected.score <- ExpectedScore(users[selected.user, "rating.estimated"], 
                                  items[selected.item, "rating.estimated"], 
                                  # items[selected.item, "discrimination.estimated"], # Use this when running math garden with variable a-parameters
                                  1 ) # Use 1 as in Math garden.
  
  # Calculate score
  score <- Score(time, deadline, correct)
  
  # Log records
  cat(correct,
      time,
      score,
      expected.score,
      users[selected.user, "rating_uncertainty"],
      items[selected.item, "rating_uncertainty"],
      users[selected.user, "k_factor"],
      items[selected.item, "k_factor"],
      "",
      file=log.records)
  
  # ------------------------------------------------------------------------------------------
  # rating uncertainty
  # ------------------------------------------------------------------------------------------
  
  # Update rating uncertainty starting at second trial
  if (trial > 1) { 
    users[selected.user, "rating_uncertainty"] <- RatingUncertaintyUser(users[selected.user, "rating_uncertainty"], expected.score, score)
    items[selected.item, "rating_uncertainty"] <- RatingUncertaintyItem(items[selected.item, "rating_uncertainty"], expected.score, score)
  }
  
  # ------------------------------------------------------------------------------------------
  # Compute K factors
  # ------------------------------------------------------------------------------------------
  
  users[selected.user, "k_factor"] <- KFactorUser(users[selected.user, "rating_uncertainty"], users[selected.user, "modified_count"])
  items[selected.item, "k_factor"] <- KFactorItem(items[selected.item, "rating_uncertainty"])

  # ------------------------------------------------------------------------------------------    
  # Update user rating
  # ------------------------------------------------------------------------------------------
  
  # Update user rating
  users[selected.user, "rating.estimated"] <- EloUpdateUser(theta.current  = users[selected.user, "rating.estimated"],
                                                            k.factor.user  = users[selected.user, "k_factor"],
                                                            score          = score,
                                                            expected.score = expected.score)

  # Log records
  cat(users[selected.user, "rating_uncertainty"],
      items[selected.item, "rating_uncertainty"],
      users[selected.user, "k_factor"], 
      items[selected.item, "k_factor"],
      users[selected.user, "rating.estimated"],
      "",
      file=log.records)

  # ------------------------------------------------------------------------------------------  
  # Update paired item ratings
  # ------------------------------------------------------------------------------------------  

  # Paired update if signs for scores and expected score differs between current
  # and previous item administration for the same user.
 
  if (selected.user == previous.user & sign(expected.score - score) * sign(previous.expected.score - previous.score)==-1) {
    
    # Paired update of items.      
    new.beta <- PairedUpdateItem(beta.current.item           = items[selected.item, "rating.estimated"],
                                 k.factor.current.item       = items[selected.item, "k_factor"],
                                 score.current.item          = score,
                                 expected.score.current.item = expected.score,
                                 beta.paired.item            = items[previous.item, "rating.estimated"],
                                 k.factor.paired.item        = items[previous.item, "k_factor"],
                                 score.paired.item           = previous.score,
                                 expected.score.paired.item  = previous.expected.score
                                 )
    
    # Store updated item ratings in item table
    items[selected.item, "rating.estimated"] <- new.beta$beta.current.item.new
    items[previous.item, "rating.estimated"] <- new.beta$beta.paired.item.new
    
    # Log record for paired update
    cat(new.beta$beta.current.item.new,
        previous.item,
        new.beta$beta.paired.item.new,
        "1\n", file=log.records)
        
    
  } else {
    
    # Log record for non paired update
    cat(NA,NA,NA,"0\n", file=log.records)
    
  }

  # Set previous vaviables
  previous.item           <- selected.item
  previous.expected.score <- expected.score
  previous.score          <- score
  previous.user           <- selected.user
  
# print(trial)
}

# Close log records
close(log.records)

# Load log records
logs <- read.csv(paste0(folder,"2_simulated_data/log_records.csv"), sep=" ", dec = ".", header = TRUE)

# Drop log records from file system
file.remove("2_simulated_data/log_records.csv")

# View(logs)

# Store log in dataframe in different files
save(logs, file = paste0(folder,'2_simulated_data/elo_data_long.Rdata'))


# Reshape long to wide and store results

## Select only relevant columns
elo_data_wide <- logs[, c("user_id","item_id","correct_answered")]

## Reshape long to wide
elo_data_wide <- reshape(elo_data_wide,
                         # There are multiple lines per user based on the number of items
                         # and we only need one line per user.
                         timevar   = "item_id",
                         idvar     = "user_id",
                         # By reshaping the dataframe form long to wide we end up with 
                         # the columns we want.
                         direction = "wide" )

## Replace newly created column string with only item numbers
names(elo_data_wide) <- gsub("correct_answered\\.", "", names(elo_data_wide))

## Replace row names with user id's
row.names(elo_data_wide) <- elo_data_wide[ ,1]

## Drop user_id column
elo_data_wide <- elo_data_wide[,-1]

## Convert elo wide to matrix
elo_data_wide <- as.matrix(elo_data_wide)

## Store matrix in different file
save(elo_data_wide, file = paste0(folder,'2_simulated_data/elo_data_wide.Rdata'))


# Store item ratings
elo.beta.estimations <- items[,c('item_id','rating.estimated','rating_uncertainty')]

save(elo.beta.estimations, file = paste0(folder,'2_simulated_data/elo_beta_estimations.Rdata'))

# Store user ratings
elo.theta.estimations <- users[,c('user_id','rating.estimated','rating_uncertainty')]

save(elo.theta.estimations, file = paste0(folder,'2_simulated_data/elo_theta_estimations.Rdata'))


# Push large file to git
# GitCommitPushLFS("2_simulated_data/elo_data_long.Rdata")