NewtonRaphson <- function(a.parameter, numerator, denominator, correct, theta, beta) {
  # Update discrimination parameter.
  #
  # Args:
  #   a.parameter : Current estimation of discrimination parameter a
  #   numerator   : Current numerator for calculating update for a
  #   denominator : Current denominator for calculating update for a
  #   correct     : Correct or incorrect answer to item 0 or 1
  #   theta       : Current user ability estimate
  #   beta        : Current item difficulty estimate
  #
  # Returns:
  #   numerator   : Updated numerator
  #   denominator : Updated denominator
  #   a.parameter : Updated discrimination parameter a
  
  # Create exponent variable for clarity in next update function
  exponent <- exp(a.parameter*(theta-beta))
  
  # Update numerator
  new.numerator   <- numerator   + (  theta   * ( correct - ( exponent / (1+exponent)   ) ) )
  
  # Update denominator
  new.denominator <- denominator + ( -theta^2 *             ( exponent / (1+exponent)^2 )   )
  
  # Update discriminaton parameter
  new.a.parameter <- a.parameter - ( numerator / denominator )
  
  returnList <- list("numerator"   = new.numerator, 
                     "denominator" = new.denominator,
                     "a.parameter" = new.a.parameter)
  
  return(returnList)
}

NewtonRaphsonCoomans <- function(a.parameter, numerator, denominator, correct, theta, beta) {
  # Update discrimination parameter.
  #
  # Args:
  #   a.parameter : Current estimation of discrimination parameter a
  #   numerator   : Current numerator for calculating update for a
  #   denominator : Current denominator for calculating update for a
  #   correct     : Correct or incorrect answer to item 0 or 1
  #   theta       : Current user ability estimate
  #   beta        : Current item difficulty estimate
  #
  # Returns:
  #   numerator   : Updated numerator
  #   denominator : Updated denominator
  #   a.parameter : Updated discrimination parameter a
  
  # Calculate differense between theta and beta
  delta = theta - beta
  
  # Create exponent variable for clarity in next update function
  exponent <- exp(a.parameter*delta)
  
  # Update numerator
  new.numerator   <- numerator   + ( delta   * ( correct - ( exponent / (1+exponent)   ) ) )
  
  # Update denominator
  new.denominator <- denominator + ( delta^2 *             ( exponent / (1+exponent)^2 )   )
  
  # Update discriminaton parameter
  new.a.parameter <- a.parameter + ( numerator / denominator )
  
  returnList <- list("numerator"   = new.numerator, 
                     "denominator" = new.denominator,
                     "a.parameter" = new.a.parameter)
  
  return(returnList)
}