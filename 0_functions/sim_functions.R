# -------------------------------------------------------------------------------------------------------
# Functions for setting up the simulations
# -------------------------------------------------------------------------------------------------------

FindBeta <- function(theta, a, p) {
  # Calculates beta based on theta, discrimination and desired probability.
  #
  # Args:
  #   theta : A Real number representing an ability estimate
  #   a     : A Real number representing the discrimination of an item
  #   p     : A probability between 0 and 1 representing the desired probability of a correct response
  #
  # Returns:
  #   beta: The difficulty of an item considering a, p and theta  
  beta = ( a * theta - log(-(p/(p-1))) ) / a
  
  return(beta)
}

# -------------------------------------------------------------------------------------------------------

TargetProbability <- function(n = 1, p = .75, sd = .1) {
  # Sample random probability from normal distribution
  #
  # Args:
  #   n  : Integer representing the number of probabilities to sample
  #   p  : Mean probability between 0 and 1
  #   sd : Standard deviation for the sampling from a normal distributeion
  #
  # Returns:
  #  p_target: Sampled probability capped.
  
  p_target <- rnorm(n,p,sd)
  
  # Cap 
  if(p_target > .99) p_target = .99
  if(p_target < .5)  p_target = .5
  
  return(p_target)
}

# ------------------------------------------------------------------------------------------------------- #

FindProbabilities <- function(theta, beta, a) {
  # Calculates probabilities based on beta's, theta's, discriminations.
  # Can be used for single or multiple probabilities
  #
  # Args:
  #   theta : A Real number representing an ability estimate
  #   beta  : The difficulty of an item
  #   a     : A Real number representing the discrimination of an item
  #
  # Returns:
  #   p: A probabilitie vector between 0 and 1 representing the desired probability of a correct response
  
  p = 1 / ( 1 + exp(-a * (theta - beta)) )
  
  return(p)
}

# ------------------------------------------------------------------------------------------------------- #

pSelection <- function(q, target_mu=.75, target_sigma=.1) {
  # Calculates scaled density based on quantiles, target mu and sigma.
  #
  # Args:
  #   q            : Vector of quantiles
  #   target_mu    : Central value to which the probability for selection should be maximal.
  #   target_sigma : The amount of variability.
  #
  # Returns:
  #   probability_for_selection: A probability between 0 and 1 representing the 
  #                              desired probability for selection.

  probability_for_selection <- dnorm(q, target_mu, target_sigma)/dnorm(target_mu, target_mu, target_sigma)
  
  return(probability_for_selection)
}


