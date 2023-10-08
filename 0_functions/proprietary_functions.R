PairedUpdateItem <- function(beta.current.item, k.factor.current.item, score.current.item,expected.score.current.item,
                             beta.paired.item,  k.factor.paired.item,  score.paired.item,  expected.score.paired.item) {
  # Update current and paired item.
  #
  # Args:
  #   beta.current.item           : Beta of current item
  #   k.factor.current.item       : K factor of current item
  #   score.current.item          : Obtained score between -1 and 1 of current item
  #   expected.score.current.item : Expected score between -1 and 1 of current item
  #   beta.paired.item            : Beta of paired item
  #   k.factor.paired.item        : K factor of paired item
  #   score.paired.item           : Obtained score between -1 and 1 of paired item
  #   expected.score.paired.item  : Expected score between -1 and 1 of paired item
  #
  # Returns:
  #   beta.current.item.new : Newly calculated beta of current item.
  #   beta.paired.item.new  : Newly calculated beta of paired item.
  
  # Update current and paired item rating. Calculate mean difference.
  mean.delta <- ( k.factor.current.item * (expected.score.current.item - score.current.item ) - 
                  k.factor.paired.item  * (expected.score.paired.item  - score.paired.item  )   ) / 2
  
  
  beta.current.item.new  <- beta.current.item + mean.delta
  beta.paired.item.new   <- beta.paired.item  - mean.delta
  
  # TODO(Sharon): Should use list for use of $ after function call
  return(list("beta.current.item.new" = beta.current.item.new, 
              "beta.paired.item.new"  = beta.paired.item.new))
}