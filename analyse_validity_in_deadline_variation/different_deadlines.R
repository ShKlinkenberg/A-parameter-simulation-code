## ---------------------------------------------------
## Effect of Different Deadlines for Table game
## ---------------------------------------------------

rm(list = ls())

library(plyr)
require("DBI");require("RMySQL")
source('~/Abe/Connect.R')
library(nnet)

setwd("~/Abe/2016 Alpha Estimation/")

## ---------------------------------------------------
## Get item table
## - Is beta related to deadline? 

items  <- dbGetQuery(con, 
                     paste("SELECT id, question, maximum_response_in_seconds, rating, modified_count, rating_uncertainty
                            FROM items
                            WHERE domain_id = 59"))

tmp <- apply(as.matrix(items$question), 1, function(x) tail(strsplit(x, split = ':')[[1]], 1))
tmp <- apply(as.matrix(tmp), 1, function(x) strsplit(x, split = '}')[[1]][1])
items$question <- apply(as.matrix(tmp), 1, function(x) strsplit(x, split = '"')[[1]][2])
items$a <- as.numeric(apply(as.matrix(items$question), 1, function(x) strsplit(x, split = ' x ')[[1]][1]))
items$b <- as.numeric(apply(as.matrix(items$question), 1, function(x) strsplit(x, split = ' x ')[[1]][2]))
items$ab <- items$a * items$b
names(items)[1] <- 'item_id'

## ---------------------------------------------------
## Estimate a with logistic regression:
## - Is alpha related to deadline?

save_all <- matrix(NA, 1, 12)
  
for(i in 1:nrow(items))
{
  cat(i, '- ')
  tmp <- dbGetQuery(con, 
                   paste("SELECT correct_answered, response_in_milliseconds, user_domain_rating, item_rating, difficulty, new_item_modified_count, new_user_domain_modified_count
                            FROM log_records_59
                            WHERE item_id = ", items$item_id[i]))
  tmp <- tmp[tmp$new_user_domain_modified_count >= 30,]
  tmp <- tmp[tmp$new_item_modified_count >= 1000,]
  
  tmp$thbe <- tmp$user_domain_rating - tmp$item_rating
#   tmp$cut_rt <- cut(tmp$response_in_milliseconds, quantile(tmp$response_in_milliseconds, seq(0, 1, .01))) 
#   foo <- na.omit(ddply(tmp, .(cut_rt), summarise, rt=mean(response_in_milliseconds), p = mean(correct_answered)))
#   plot(foo$rt, foo$p)
#   
  mod_tmp <- glm(correct_answered ~ thbe, data = tmp, family = binomial(link = "logit"))
  save <- c(items$item_id[i], coefficients(mod_tmp), AIC(mod_tmp), nrow(tmp), 
            mean(tmp$correct_answered[tmp$difficulty == 0]),
            mean(tmp$correct_answered[tmp$difficulty == 1]),
            mean(tmp$correct_answered[tmp$difficulty == 2]),
            mean(tmp$response_in_milliseconds[tmp$difficulty == 0]),
            mean(tmp$response_in_milliseconds[tmp$difficulty == 1]),
            mean(tmp$response_in_milliseconds[tmp$difficulty == 2]),
            sd(tmp$item_rating))
  save_all <- rbind(save_all, save)
}

save_all <- as.data.frame(save_all)
names(save_all) <- c('item_id', 'intercept', 'alpha', 'AIC', 'n_responses', 'p0', 'p1', 'p2', 'rt0', 'rt1', 'rt2', 'sdBeta')
all <- join(items, save_all)
save(all, file='dataTableGame.Rdata')
