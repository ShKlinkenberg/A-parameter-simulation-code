# ----------------------------------------------------------------------------------------------
# Calculate a-parameters with GLM logistic function
# ----------------------------------------------------------------------------------------------

glmEstimations <- function(dat, method, scenario, folder) {

  # ----------------------------------------------------------------------------------------------
  # GLM estimations
  # ----------------------------------------------------------------------------------------------
  
  # Setup result matrix
  unique.item.ids <- unique(dat$item_id)
  parameters      <- matrix(NA, length(unique.item.ids), 3)
  
  # Loop through all items
  for(item in 1:length(unique.item.ids)) {
    
    # Create subset for selected item
    sel <- dat[dat$item_id == unique.item.ids[item],]
    
    # Only run if sufficient ammount of responses
    if(nrow(sel) > 100) {
      
      # Calculate delta
      sel$delta <- sel$user_domain_rating - sel$item_rating
      
      # Fit model without intercept just one B for delta
      mod <- glm(correct_answered ~ 0 + delta, family=binomial(link='logit'), data=sel)
      
      # Store parameter and fit measure
      parameters[item,1] <- unique.item.ids[item]
      parameters[item,2] <- coefficients(mod)
      parameters[item,3] <- AIC(mod)
      
    }
  }
  
  parameters <- as.data.frame(parameters)
  
  names(parameters) <- c("item_id", "glm.X.Y.a", "glm.X.Y.aic")
  
  names(parameters) <- gsub("X", method,   names(parameters))
  names(parameters) <- gsub("Y", scenario, names(parameters))
  
  glm.name <- paste0("glm.",method,".estimations"); assign(glm.name, parameters) 
  
  # Save method objects to method file
  save(list = c(glm.name),
       file = paste0(folder,"4_glm/glm_",method,".Rdata") )
  
} # end glmEstimation function