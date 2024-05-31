## Slide 12

muskox <- read.csv("https://tinyurl.com/42h2u7bm")

install.packages("moveHMM") # if not already installed
library(moveHMM)
data <- prepData(muskox, type = "UTM")

head(data)

plot(data)

## Slide 43

faithful <- read.table("https://tinyurl.com/y6m9b5be")
mllk <- function(theta, x){
  mu <- theta[1:2]
  sigma <- theta[3:4]
  pi <- theta[5]
  logl <- sum(log(pi * dnorm(x, mu[1], sigma[1]) + (1 - pi) * dnorm(x, mu[2], sigma[2])))
  return(-logl)
}

mod <- nlminb(c(50, 80, 5, 7, 0.3), mllk, x = faithful$waiting,
              lower = c(0, 0, 0, 0, 0), upper = c(Inf, Inf, Inf, Inf, 1))

mod$par

## Slide 58

# delta <- solve(t(diag(N) - Gamma + 1), rep(1, N))

Gamma <- matrix(c(0.9, 0.1, 0.2, 0.8), nrow = 2, byrow = TRUE)
delta <- solve(t(diag(2) - Gamma + 1), c(1, 1))
delta

delta %*% Gamma
