# ------------------------------------------------------------------------------------------------------- #
# Functions for running the Elo adaptive procedure
# ------------------------------------------------------------------------------------------------------- #

TargetProbability <- function(mu, sigma, n = 1) {
  # Select random probability from normal distribution with difficulty settings 
  # expressed as mu and sigma difficulty.
  #
  # Args:
  #   mu    : Difficulty. Probability of answering correct between 0 (difficult) and 1 (eesy).
  #   sigma : Allowed variability of sampled probability.
  #   n     : number of probabilities to sample. Default is 1
  #
  # Returns:
  #   target.probability : Sampled probability between 0 and 1
  
  target.probability <- rnorm(n, mu, sigma)
  
  # Make sure sampled value is between 0 and 1
  if (target.probability < .0) target.probability <- .0
  # If sampled value exceeds 1 then set to mu
  # Could also be .99
  if (target.probability > 1 ) target.probability <- mu
  
  return(target.probability)
}

# ------------------------------------------------------------------------------------------------------- #

TargetProbabilityToBetas <- function(TargetProbability, discriminations, theta) {
  # Calculate target betas based on a specific probability of correctly answering 
  # an item with all discrimination parameters a.
  # Solves logistic 2PL function for betas.
  #
  # Args:
  #   TargetProbability : Single probability value between 0 and 1
  #   discriminations   : Discrimination parameters a for the 2PL logistic function
  #                       Input is a vector with all known a parameters for betas 
  #   theta             : ability estimate theta of the user
  #
  # Returns:
  #   target.beta : Difficulty beta corresponding to the probability, discrimination and theta
  
  target.betas <- -1 * (1/discriminations) * log(TargetProbability/(1-TargetProbability)) + theta
  
  return(target.betas)
}

# ------------------------------------------------------------------------------------------------------- #

SelectItemFromTargetBetas <- function(target.betas, all.estimated.betas, all.item.frequencies, items.to.exclude, number.of.closest.items = 5) {
  # Select item based on absolute distance to target difficultie beta, restricted to set number of closest items.  
  # Select item with lowest frequency
  #
  # Args:
  #  target.betas            : Vector of all target betas
  #  all.estimated.betas     : Vector of all current betas
  #  all.item.frequencies    : Vector with all item frequencies
  #  items.to.exclude        : Vector of items to exclude
  #  number.of.closest.items : Subset of closest items
  #
  # Returns:
  #   selected.item : Item number / id as natural number

  # Set items to to exclude to Infinit as not to select them for sampling
  all.estimated.betas[ items.to.exclude ] <- Inf
  
  # Select number of candidates which target beta closest to the real beta
  candidates.item.numbers <- order(abs(target.betas - all.estimated.betas))[1:number.of.closest.items] 
  
  # From sub selection select item number with lowest administration frequency
  selected.item <- candidates.item.numbers[ which.min( all.item.frequencies[candidates.item.numbers] ) ]
  
  return(selected.item)
}

# ------------------------------------------------------------------------------------------------------- #

SampleResponse <- function(theta, beta, a.parameter) {
  # Sample a response from binomial distribution
  #
  # Args:
  #   theta       : Ability real number
  #   beta        : Difficulty real number
  #   a.parameter : Discrimination real number
  #
  # Returns:
  #   correct : Binary response 0 or 1
  
  # Sample response from binomial distribution
  correct <- rbinom(1, 1, 1 / ( 1+exp(-a.parameter*(theta - beta)) ))
  
  return(correct)
}

# ------------------------------------------------------------------------------------------------------- #

SampleResponseTime <- function(correct, theta, beta, a.parameter, deadline) {
  # Sample response time for correct and incorrect answers
  #
  # Args:
  #   correct     : Binary response 0 or 1
  #   theta       : Ability real number
  #   beta        : Difficulty real number
  #   a.parameter : Discrimination real number
  #   deadline    : Time limit in seconds
  #
  # Returns:
  #   response.time: Sampled response time in seconds.
  
  delta = theta - beta
  
  # Sample response time when response is:
  if (correct == 1) {
    # Correct
    response.time <- -log( 1 - runif(1) * (1-exp(-a.parameter*delta))     ) / (delta*a.parameter / deadline)
  } else {
    # Incorrect
    response.time <-  log(     runif(1) * (  exp(a.parameter*delta)-1) +1 ) / (delta*a.parameter / deadline)
  }
  
  return(response.time)
}

# ------------------------------------------------------------------------------------------------------- #

ExpectedScore <- function(estimated.theta, beta, a.parameter) {
  # Calculate expected score based on ability and difficulty and the discrimination
  # of the item
  #
  # Args:
  #   estimated.theta : Estimated ability real number
  #   beta            : Difficulty real number
  #   a.parameter     : Discrimination real number
  #
  # Returns:
  #   expected.score : Real number between -1 and 1
  
  delta = estimated.theta - beta 
  
  if (delta == 0) { # If theta - beta is exactly zero then expected score should be zero
    expected.score <- 0 
  } else {
    expected.score <- (a.parameter*(exp(2*a.parameter*delta)+1) /
                                   (exp(2*a.parameter*delta)-1) - (1/delta))/a.parameter
  }
  
  return(expected.score)
}

# ------------------------------------------------------------------------------------------------------- #

Score <- function(response.time, deadline, correct) {
  # Calculate high speed high stakes score
  #
  # Args:
  #   response.time : Response time in seconds
  #   deadline      : Maximum alowed time in seconds
  #   correct       : Binary response 0 or 1
  #
  # Returns:
  #   score : Real number between -1 and 1
  
  # Calculate score
  if (response.time > deadline) {
    # No score if response time is over the deadline
    score <- 0 
  } else {
    # Score calculation based on HSHS scoring rule
    score <- (2*correct-1) * (deadline-time)/deadline
  }
  
  return(score)
}

# ------------------------------------------------------------------------------------------------------- #

RatingUncertaintyItem <- function(rating.uncertainty.item.old, expeced.score, score, weight = .2) {
  # Calculate rating uncertainty for items.
  #
  # Args:
  #   rating.uncertainty.item.old : Previous uncertainty for the item. Real number
  #   expeced.score               : Expected score between -1 and 1
  #   score                       : Obtained score between -1 and 1
  #   weight                      : Weight dispersion on updating scheme 
  #
  # Retursn:
  #   rating.uncertainty.item.old : Updated uncertainty for the item
  
  #                              Decrease previous uncertainty              Decrease if score is better than expected
  rating.uncertainty.item.new <- (1-weight) * rating.uncertainty.item.old + weight * sign(expeced.score - score)
  
  return(rating.uncertainty.item.new)
}

# ------------------------------------------------------------------------------------------------------- #

RatingUncertaintyUser <- function(rating.uncertainty.user.old, expeced.score, score, weight = .2) {
  # Calculate rating uncertainty for users.
  #
  # Args:
  #   rating.uncertainty.user.old : Previous uncertainty for the user. Real number
  #   expeced.score               : Expected score between -1 and 1
  #   score                       : Obtained score between -1 and 1
  #   weight                      : Weight dispersion on updating scheme 
  #
  # Retursn:
  #   rating.uncertainty.user.new : Updated uncertainty for the user
  
  #                              Decrease previous uncertainty              Decrease if score is worse than expected
  rating.uncertainty.user.new <- (1-weight) * rating.uncertainty.user.old + weight * sign(score - expeced.score)
  
  return(rating.uncertainty.user.new)
}

# ------------------------------------------------------------------------------------------------------- #

KFactorItem <- function(rating.uncertainty.item, k.factor.constant = 1, k.power.constant = 5, k.floor = .001) {
  # Calculate new K factor for items.
  #
  # Args:
  #   rating.uncertainty.item : Current rating uncertainty of item. Real number
  #   k.factor.constant       : K factor constant. Real number
  #   k.power.constant        : Damping factor for k. Real number
  #   k.floor                 : Minimal value for k
  #
  # Returns:
  #   k.factor.item : Newly calculated K factor for the item.

  k.factor.item <- max(k.floor, k.factor.constant * abs(rating.uncertainty.item) ^ k.power.constant)
  
  return(k.factor.item)
}

# ------------------------------------------------------------------------------------------------------- #

KFactorUser <- function(rating.uncertainty.user, frequency.user, frequency.limit = 20, frequency.weight = .2, k.factor.constant = 1, k.power.constant = 5, k.floor = .001) {
  # Calculate new K factor for users.
  #
  # Args:
  #   rating.uncertainty.user : Current rating uncertainty of user. Real number
  #   frequency.user          : Total number of administrations for user. Whole number.
  #   frequency.limit         : Increase limit to low frequency
  #   frequency.weight        : Increase of k-factor for low frequency
  #   k.factor.constant       : K factor constant. Real number
  #   k.power.constant        : Damping factor for k. Real number
  #   k.floor                 : Minimal value for k
  #
  # Returns:
  #   k.factor.user : Newly calculated K factor for the user.
  
  k.factor.user <- max(k.floor, k.factor.constant * abs(rating.uncertainty.user) ^ k.power.constant + frequency.weight * (frequency.user <= frequency.limit))
  
  return(k.factor.user)
}

# ------------------------------------------------------------------------------------------------------- #

EloUpdateUser <- function(theta.current, k.factor.user, score, expected.score) {
  # Calculate new theta
  #
  # Args:
  #   theta.current  : Current theta. Real number
  #   k.factor.user  : K factor for the user
  #   score          : Obtained score between -1 and 1
  #   expected.score : Expected score between -1 and 1
  #
  # Returns:
  #   theta.new : Newly calculated theta
  
  theta.new <- theta.current + k.factor.user * (score - expected.score)
  
  return(theta.new)
}

# ------------------------------------------------------------------------------------------------------- #

source("0_functions/proprietary_functions.R")

# ------------------------------------------------------------------------------------------------------- #

ExcludedItemRing <- function(excluded.items, item) {
  # Update excluded item ring. Add current item and delete oldest item
  #
  # Args:
  #   excluded.items : Vector of current items to be excluded.
  #   item           : Currently selected item.
  #
  # Returns:
  #   excluded.items.update : New vector of items to be excluded form selection.
  
  # Subtract oldest item and add new item to vector
  excluded.items.update <- c(excluded.items[-1], item)
  
  return(excluded.items.update)
}