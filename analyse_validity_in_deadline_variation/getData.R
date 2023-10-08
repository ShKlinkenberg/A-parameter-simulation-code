# -----------------------------------------------------
# 2016: CogDev
# -----------------------------------------------------

rm(list = ls())

source("~/Abe/Settings.R")
source("~/Abe/Connect.R")
source('~/shared-project-01/AgeCheckFunction.R')

library(plyr)

setwd('~/Abe/2016 Alpha Estimation/')

# -----------------------------------------------------
# Data selection for CogDev. Project
# - Get frequent players
# - Stable ability within data collection period
# - Domains:
# (1) Game addition
# (2) Game fastmix, test seperate dimensions?
#
# Open issues:
# (1) Vraagtekens (eerst eruit!)
#     Too late (eerst eruit!)
# (2) Moeilijkheid (eerst alleen medium!)
# (3) Clean RT's? (eerst nog niet)
#
# Nu kijken we nog naar alle data van twee maanden, misschien later nog data selecteren zodat
# we van elke kind alleen maar data binnen 1 dag/week/maand hebben. Nu zitten hier mogelijk nog
# grote verschillen tussen kinderen.

# Misschien nog kinderen eruit die grote spreiding hebben in rating in de data periode
#
# -----------------------------------------------------

# Settings:

times <- 3 # 3 time points
domains <- 59

## Get General User Info

# us_info <- dbGetQuery(con, " SELECT id, gender, grade, date_of_birth, modified FROM users ")
# names(us_info)[1] <- 'user_id'

# Select: Grade, Created, Age-Check:

#us_info <- us_info[us_info$grade %in% grades,]
#sel <- age.check(date_of_birth = us_info$date_of_birth, grade = us_info$grade, date_of_play = us_info$modified)
#us_info <- us_info[sel,]

# -----------------------------------------------------

min_times <- c( "2013-09-01 00:00:00", "2014-09-01 00:00:00", "2015-09-01 00:00:00")
max_times <- c( "2014-03-01 00:00:00", "2015-03-01 00:00:00", "2016-03-01 00:00:00")

for(d in domains)
{
  for(t in 3) # 1:times
  {
    
    min_created <- min_times[t]
    max_created <- max_times[t]
    
    ## Get Log:
    
    query <- paste0("SELECT user_id, item_id, correct_answered, response_in_milliseconds, created, answer, new_user_domain_modified_count, difficulty, user_domain_rating, item_rating
                      FROM log_records_", d, "
                      WHERE created > '", min_created, "'
                      AND created <= '", max_created, "'")
    
    dat <- dbGetQuery(con,query)
    
    # Person eruit met veel ?
    
    x <- ddply(dat, .(user_id), summarize, q=mean(answer == '¿'))
    dat1 <- dat[dat$user_id %in% x$user_id[x$q <= .1],]
    
    # Data Selection:
    
    #     dat1 <- dat1[dat1$user_id %in% us_info$user_id,]
    dat1 <- dat1[dat1$answer != '¿',] 
    dat1 <- dat1[dat1$answer != '…',] 
    dat1 <- dat1[dat1$difficulty == 1,] 
    
    # Only players with more than min_items:
    
    dat1 <- dat1[dat1$new_user_domain_modified_count >= 45,] 
    
    x <- ddply(dat1, .(user_id), summarize, n = length(correct_answered))
    sel <- x$user_id[x$n >= 45]
    dat <- dat1[dat1$user_id %in% sel,]
    
    # If domain = 59 add deadlines:
    if(d == 59)
    {
      query <- paste0("SELECT id, maximum_response_in_seconds
                      FROM items
                      WHERE domain_id = 59")
      it <- dbGetQuery(con,query)
      names(it) <- c('item_id', 'deadline')
      dat <- join(dat, it)
    }
    
    # Final Data:
    
    dat <- dat[order(dat$user_id, dat$item_id, dat$created),]
    save(dat, file=paste0('datDomain', d, 'time', t, '.Rdata'))
    
  }
}

# -----------------------------------------------------
