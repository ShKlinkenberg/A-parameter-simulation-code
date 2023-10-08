# -----------------------------------------------------
# 2016: CogDev
# -----------------------------------------------------

rm(list = ls())

source("~/Abe/Settings.R")
source("~/Abe/Connect.R")

library(plyr)

setwd('~/Abe/2016 Alpha Estimation/')

# -----------------------------------------------------

load(file='datDomain3time1.Rdata')
dat1 <- dat
load(file='datDomain3time2.Rdata')
dat2 <- dat
load(file='datDomain3time3.Rdata')
dat3 <- dat

# -----------------------------------------------------

its <- unique(dat$item_id)

foo <- dbGetQuery(con, " SELECT id, question FROM items")
foo <- foo[foo$id %in% its,]
foo$q <- apply(as.matrix(foo$q), 1, function(x) strsplit(x, split=':')[[1]][[6]])
foo$q <- apply(as.matrix(foo$q), 1, function(x) strsplit(x, split='}}')[[1]][[1]])

# -----------------------------------------------------
# Methode Abe:
# -----------------------------------------------------

for(j in 1:3)
{
  
  if(j == 1) dat <- dat1
  if(j == 2) dat <- dat2
  if(j == 3) dat <- dat3
  
  its <- unique(dat$item_id)
  pars <- matrix(NA, length(its), 5)
  
  for(i in 1:length(its))
  {
    sel <- dat[dat$item_id == its[i],]
    if(nrow(sel) > 100)
    {
      sel$thbe <- scale(sel$user_domain_rating - sel$item_id)
      mod <- glm(correct_answered ~ thbe, family=binomial(link='logit'), data=sel)  
      pars[i,1:2] <- coefficients(mod)
      pars[i,3] <- AIC(mod)
      pars[i,4] <- nrow(sel)
      pars[i,5] <- mean(sel$item_rating)
    }
  }
  
  pars <- as.data.frame(pars)
  pars$id <- its
  pars <- na.omit(pars)
  names(pars)[-6] <- paste(c('b0', 'b1', 'AIC', 'N', 'be'), j, sep='_')
  
  if(j == 1) pars1 <- pars
  if(j == 2) pars2 <- pars
  if(j == 3) pars3 <- pars
  
}

x <- join(pars1, foo[,c('id', 'q')])
x <- join(x, pars2)
x <- join(x, pars3)

layout(matrix(1:6, 3, 2))
plot(x$b1_1, x$b1_2)
plot(x$b1_1, x$b1_3)
plot(x$b1_2, x$b1_3)

cor(x[,-(6:7)], use='pair')
pairs(x[,-(6:7)]), use='pair')

# -----------------------------------------------------
# Methode Gunter:
# -----------------------------------------------------

for(j in 1:3)
{
  
  if(j == 1) dat <- dat1
  if(j == 2) dat <- dat2
  if(j == 3) dat <- dat3
  
  dat <- dat[order(dat$user_id, dat$item_id, dat$created),]
  
  check <- data.frame(us0 = dat$user_id[-nrow(dat)],
                      us1 = dat$user_id[-1],
                      it0 = dat$item_id[-nrow(dat)],
                      it1 = dat$item_id[-1],
                      t0 = dat$correct_answered[-nrow(dat)],
                      t1 = dat$correct_answered[-1],
                      rat_p = dat$user_domain_rating[-nrow(dat)],
                      rat_i = dat$item_rating[-nrow(dat)],
                      time0 = dat$created[-nrow(dat)],
                      time1 = dat$created[-1])
  
  check <- check[check$us0 == check$us1 & check$it0 == check$it1,]
  t <- as.numeric(difftime(check$time1, check$time0, unit='days'))
  
  # check$times <- cut(t, quantile(t, seq(0, 1, .05)))
  # foo <- ddply(check[check$t0 == 0,], .(times), summarize, p = mean(t1))
  # plot(foo)
  
  check <- check[t < 7,]
  
  # -----------------------------------------------------
  # alpha items:
  
  pars <- ddply(check, .(it0), summarize, n=length(t0), p=mean(t0))
  pars$a <- numeric(nrow(pars))
  #pars$learning <- numeric(nrow(pars))
  
  for(p in 1:nrow(pars)){
    temp <- check[check$it0 == pars$it0[p],]
    t <- table(temp$t0, temp$t1)
    pars$a[p] <- sum(diag(t))/sum(t)
    #pars$learning[p] <- sum(diag(t))/sum(t)
  }
  
  pars <- na.omit(pars)
  names(pars)[-1] <- paste(c('n', 'p', 'a'), j, sep='_')
  names(pars)[1] <- 'id'
  
  if(j == 1) pars1 <- pars
  if(j == 2) pars2 <- pars
  if(j == 3) pars3 <- pars
}

x_gunter <- join(pars1, foo[,c('id', 'q')])
x_gunter <- join(x_gunter, pars2)
x_gunter <- join(x_gunter, pars3)

x_gunter <- x_gunter[x_gunter$n_1 > 100 & x_gunter$n_2 > 100 & x_gunter$n_3 > 100,]

layout(matrix(1:6, 3, 2))
plot(x_gunter$a_1, x_gunter$a_2)
plot(x_gunter$a_1, x_gunter$a_3)
plot(x_gunter$a_2, x_gunter$a_3)
cor(x_gunter[,-5])

all <- join(x, x_gunter)

cor(all$b1_1, all$a_1, use = 'pair')
cor(all$b1_2, all$a_2, use = 'pair')
cor(all$b1_3, all$a_3, use = 'pair')
cor(all[-7], use = 'pair')
