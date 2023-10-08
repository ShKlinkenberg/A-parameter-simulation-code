# ----------------------------------------------------------------------------------------------
# Calculate a-parameters with GLM logistic function 2 parameters
# ----------------------------------------------------------------------------------------------

# Clear memory
rm(list = ls())

# Search for files in folder real_data containing "datDomain3time" 
# followed by any character.
files     <- list.files("analyse_validity_in_deadline_variation/1_data_selection/", pattern = "Rdata")
# substitude name by relevant output file name
file.name <- gsub("log", "glm_2par_data_estimaton_", files)

for(i in 1:length(files)) {

  # Load data
  load(paste0("analyse_validity_in_deadline_variation/1_data_selection/", files[i]))

  # ----------------------------------------------------------------------------------------------
  # GLM estimations
  # ----------------------------------------------------------------------------------------------
  
  # Setup result matrix
  unique.item.ids           <- unique(dat$item_id)
  parameters                <- data.frame(item_id = unique.item.ids)
  parameters$glm.2pl.data.a <- NA
  parameters$glm.2pl.data.n <- NA

  # Set start time
  time.start  <- Sys.time()
  
  # Loop through all items
  for(item in unique.item.ids) {
    
    # Create subset for selected item
    sel <- dat[dat$item_id == item,]
    
    # Only run if sufficient ammount of responses
    if(nrow(sel) > 100) {
      
      # Calculate delta
      # sel$delta <- scale(sel$user_domain_rating - sel$item_rating)
      sel$delta <- sel$user_domain_rating - sel$item_rating
      
      # Fit model with intercept
      mod <- glm(correct_answered ~ delta, family=binomial(link='logit'), data=sel)
      
      # Store parameter and fit measure
      # parameters[parameters$item_id == item, 2] <- coefficients(mod)['(Intercept)']
      parameters[parameters$item_id == item, 2] <- coefficients(mod)['delta']
      # parameters[item,3] <- AIC(mod)
      
    }
    
    parameters[parameters$item_id == item, 3] <- nrow(sel)
    
  }
  
  var.names <- gsub("data", paste0("data.",i), names(parameters))
  names(parameters) <- var.names
  glm.2par.data.estimations <- parameters
  
  # Determine time
  time.glm.2par <- Sys.time() - time.start
  
  # Add deadline to estimations
  glm.2par.data.estimations <- merge(glm.2par.data.estimations, unique(dat[,c("item_id",'deadline')]), by = 'item_id', all.x = TRUE)
  
  save(glm.2par.data.estimations,
       time.glm.2par,
       file = paste0("analyse_validity_in_deadline_variation/2_glm_2par/", file.name[i])
       )

} # End loop through data sets