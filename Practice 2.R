# 1. 2.7 would be a reasonable estimation
# 2
library(knitr)
library(tidyverse)
# dataset
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))
# distance
# ignore the yvalues
plot(dat)
dat$d <- abs(dat$x-0)  # only look at x distance, look back to stock
dat$d
dat2 <- arrange(dat,d) # arrange by the second argument,
dat2
dat2_final <- dat2[1:5,]
dat2_final
ypre <- mean(dat2_final$y)
ypre
# loess
dat_loess <- filter(dat2,d<0.01)
mean(dat_loess$y)
# when r is small, we may overfit the data

library(ggplot2)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  dat2 <- arrange(dat,d)
  dat_final <- dat[1:5,]
  return(ypre <- mean(dat2_final$y))

  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  dat2 <- arrange(dat,d)
  dat_final <- dat[1:5,]
  dat_loess <- filter(dat2, d<1)
  return(mean(dat_loess$y))
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="blue",size=1.5) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method,size=0.5)) +
  theme_bw()


