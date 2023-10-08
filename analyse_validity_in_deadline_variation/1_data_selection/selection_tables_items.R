# -------------------------------------------------------------------
# Subselection high frequency items
#
# WARNING: This query can only be run on the oefenweb Rstudio server
# -------------------------------------------------------------------

# Select only items with high probability of having reliable scores

# Clear memory
rm(list = ls())

# library('devtools')
# install_github("Oefenweb/r-database")
# install.packages('plyr')

library('oefenwebDatabase')
library('plyr')

con <- connect()

query = "
SELECT log_records_59.id,
       log_records_59.user_id,
       log_records_59.item_id,
       log_records_59.user_domain_rating,
       log_records_59.item_rating,
       log_records_59.correct_answered,
       log_records_59.answer,
       log_records_59.grade,
       log_records_59.new_item_modified_count,
       log_records_59.new_user_domain_modified_count,
       log_records_59.new_user_domain_rating_uncertainty,
       log_records_59.new_item_rating_uncertainty,
       log_records_59.difficulty,
       items.maximum_response_in_seconds as deadline
FROM   log_records_59, items
WHERE  log_records_59.item_id = items.id
AND    new_item_modified_count            > 50
AND    new_user_domain_modified_count     > 50
AND    difficulty = 1                         
"

# years = c(2014, 2015, 2016)

# for(year in years) {
  
  # q <- sprintf(query, year, year+1)
  
  dat <- DBI::dbGetQuery(con, query)
  
  # Select only most recent answers
  # most.recent <- aggregate(id ~ user_id + item_id, data = dat, max)$id
  # 
  # dat <- dat[which(dat$id %in% most.recent),]
  
  # Select only users with low amounts of questionmark (?) use
  x   <- ddply(dat, .(user_id), summarize, q=mean(answer == '¿'))
  dat <- dat[dat$user_id %in% x$user_id[x$q <= .1],]
  
  # Get rid of questionmark (¿) and to late answers (…)
  dat <- dat[dat$answer != '¿',] 
  dat <- dat[dat$answer != '…',]
  # dat <- dat[dat$difficulty == 1,]
  
  # Subset to only users with more than 45 answers left
  # dat <- dat[dat$new_user_domain_modified_count >= 45,] 
  # 
  # x   <- ddply(dat, .(user_id), summarize, n = length(correct_answered))
  # sel <- x$user_id[x$n >= 45]
  # dat <- dat[dat$user_id %in% sel,]
  
  # Save results
  # save(dat, file = paste0("logTable", year, ".Rdata") )
  save(dat, file = "logTable.Rdata" )
  
# }

close_connection(con)
