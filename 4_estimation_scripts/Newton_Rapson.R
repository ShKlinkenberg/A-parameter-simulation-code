# --------------------------------------------------------------------------
# Newton Rapson method
# --------------------------------------------------------------------------

nrEstimations <- function(dat, method, scenario, folder) {

# Get Newton Raphson function
source("0_functions/newton_raphson_function.R")

start.time <- Sys.time()

    # Delete un used columns for speed increase
    dat <- as.matrix(dat[, c("item_id", "correct_answered", "user_domain_rating", "item_rating")])
  
    # Create item table with item numbers
    item.table <- data.frame(item_id = unique(dat[, "item_id"]))
    
    # Set initial a parameter
    initiol.a       = .5
    item.table$nr.a = initiol.a
    
    # Set restricted range for a-parameter estimation
    lower.bound.a <- -4
    upper.bound.a <- 4

    # Set initial numerator, denominator and counter
    initial.numerator         = .0001
    initial.denominator       = .0001
    initial.counter           = 0
    item.table$nr.numerator   = initial.numerator  
    item.table$nr.denominator = initial.denominator 
    item.table$nr.counter     = initial.counter
    
    # --------------------------------------------------------------------------
    # Iterate through data and update a.
    # --------------------------------------------------------------------------
    
    # Set number of iterations needed
    number.of.iterations = dim(dat)[1]
    
    # Start loop for Newton-Rapson iterations
    for(i in 1:number.of.iterations) { 
        
        # Set progress meter
        progress <- i / number.of.iterations
        # Print progress 
        print( paste("Dataset:",method, 
                     "at",sprintf("%s%%", round(progress*100,2) ),
                     "in:", round(difftime(Sys.time(), start.time, units = "mins"),2), "min" ) ) 
            
        # Get row from simulated responses
        # row <- dat[i,]
        
        # Put results from simulated data in termporary variables
        item    <- dat[i, 'item_id']
        correct <- dat[i, 'correct_answered']
        theta   <- dat[i, 'user_domain_rating']
        beta    <- dat[i, 'item_rating']

# [TODO]Sharon: item does noet retrieve the correct row in the item table
#               replace with subset(item.table, item_id == item, select = "nr.counter")
#               or item.table[which(item.table$item_id == item), 'nr.counter']
                
        # # Put current values in item table in temporary variables
        # counter      <- item.table[item, 'nr.counter']     # subset(item.table, item_id == item, select = "nr.counter")
        # a            <- item.table[item, 'nr.a']
        # numerator    <- item.table[item, 'nr.numerator']
        # denominator  <- item.table[item, 'nr.denominator']
        
# [TODO]Sharon: Make this faster. Looking up all individual variables is to slow.
#               Suggestion: item.table.row <- item.table[which(item.table$item_id == item), ]
        
        # Put current values in item table in temporary variables
        # counter      <- item.table[which(item.table$item_id == item), 'nr.counter']
        # a            <- item.table[which(item.table$item_id == item), 'nr.a']
        # numerator    <- item.table[which(item.table$item_id == item), 'nr.numerator']
        # denominator  <- item.table[which(item.table$item_id == item), 'nr.denominator']
        
        item.table.row <- item.table[which(item.table$item_id == item), ]
        
        # Put current values in item table in temporary variables
        counter     <- item.table.row$nr.counter
        a           <- item.table.row$nr.a
        numerator   <- item.table.row$nr.numerator
        denominator <- item.table.row$nr.denominator
        
        # Execute Newton Raphson function
        NewtonRaphsonCoomans(a.parameter = a,
                             numerator   = numerator,
                             denominator = denominator,
                             correct     = correct,
                             theta       = theta,
                             beta        = beta) -> ResultNewtonRaphsonFunction
        
        # Update a parameter every 200 answers
        if(counter %% 200 == 0) { 
          
          # Update current a parameter estimation
          # a <- ResultNewtonRaphsonFunction$a.parameter
          
          # Only update the a parameter if a is not NaN
          # and the newly estimated a is within restricted range
          # as not to drift away
          if (!is.na(ResultNewtonRaphsonFunction$a.parameter) &
              ResultNewtonRaphsonFunction$a.parameter < upper.bound.a &
              ResultNewtonRaphsonFunction$a.parameter > lower.bound.a ) {

            # Update current a parameter estimation
            a <- ResultNewtonRaphsonFunction$a.parameter

          } else {
            
            # Reset a-estimation
            a <- initiol.a
               
          }

          # Reset numerator and denominator
          numerator   <- initial.numerator
          denominator <- initial.denominator
          
        } else {
          
          # Update numerator and denominator
          numerator   <- ResultNewtonRaphsonFunction$numerator
          denominator <- ResultNewtonRaphsonFunction$denominator
          
        }
        
        # Update counter
        counter       = counter + 1
 
# [TODO]Sharon: item does not represent the correct row        
               
        # Store results
        # numerator    -> item.table[item, 'nr.numerator']
        # denominator  -> item.table[item, 'nr.denominator']
        # 
        # counter      -> item.table[item, 'nr.counter']
        # a            -> item.table[item, 'nr.a']
        
        # Put current values in item table in temporary variables

        # numerator    -> item.table[which(item.table$item_id == item), 'nr.numerator']
        # denominator  -> item.table[which(item.table$item_id == item), 'nr.denominator']
        # 
        # counter      -> item.table[which(item.table$item_id == item), 'nr.counter']
        # a            -> item.table[which(item.table$item_id == item), 'nr.a']
        
        item.table[which(item.table$item_id == item), c('nr.numerator',
                                                        'nr.denominator',
                                                        'nr.counter',
                                                        'nr.a')] <- c(numerator,
                                                                      denominator,
                                                                      counter,
                                                                      a)

    } # Stop loop for Newton-Rapson iterations
    
    # item.table$a = .5 + ( item.table$nr.numerator / item.table$nr.denominator )
    
    # hist(item.table[which(item.table$nr.counter > 50), "nr.a"], breaks=100)
    # 
    # plot(item.table$nr.a, item.table$nr.counter)
    
    nr.estimations <- item.table[,c("item_id","nr.a","nr.counter")]
    
    names(nr.estimations) <- gsub("\\.", paste0("\\.",method,"\\.",scenario,"\\."), names(nr.estimations))
    
    nr.name <- paste0("nr.",method,".estimations"); assign(nr.name, nr.estimations) 
    
    # Save method objects to method file
    save(list = c(nr.name),
         file = paste0(folder,"5_newton_raphson/nr_",method,".Rdata") )

} # end loop for different data sets