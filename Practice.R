
library(tidyverse)

# Exercise 1
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}
genreg(10)
dat <- genreg(2000)

dat <- mutate(dat,
              yhat=5,
              yhat1=5-x1,
              yhat2=5+2*x2,
              yhat12=5-x1+2*x2)

(mse <- mean((dat$yhat - dat$y)^2))
(mse1 <- mean((dat$yhat1 - dat$y)^2))
(mse2 <- mean((dat$yhat2 - dat$y)^2))
(mse12 <- mean((dat$yhat12 - dat$y)^2))

# Exercise 2
gencla <-function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}
dat2 <- gencla(2000)
# x=1
pB <- 0.8/(1+exp(-1))
pB
pC <- 1-pA-pB
pC
# x=2
pB <- 0.8/(1+exp(-2))
pB
pC <- 1-pA-pB
pC
# care about the mode-the highest probability
dat2 <- mutate(dat2,
              y1=y)
plot(dat2$y,dat2$x)

# table <- table(dat2$y)
# table
# pA=307/2000
# pA
# pB=688/2000
# pB
# pC=1005/2000
# pC
# 
# plot()
gencla <-function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(t)  # x is a dummy variable, just call it t
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, t, 1-t-0.2)))
  tibble(x=x, y=y)
}
dat2 <- mutate(dat2,
               yhat = sapply(x, function(x_)
                 if (x_<0) "C" else "B"))
               
#5
mean(dat2$yhat == dat2$y)