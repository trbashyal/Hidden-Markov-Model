## Slide 100

L <- function(theta, x){
  Gamma <- diag(theta[1:2])
  Gamma[1, 2] <- 1 - Gamma[1, 1]
  Gamma[2, 1] <- 1 - Gamma[2, 2] 
  delta <- solve(t(diag(2) - Gamma + 1), c(1, 1))
  lambda <- theta[3:4]
  foo <- delta %*% diag(c(dpois(x[1], lambda[1]), dpois(x[1], lambda[2])))
  for (t in 2:length(x)){
    foo <- foo %*% Gamma %*% diag(c(dpois(x[t], lambda[1]), dpois(x[t], lambda[2])))
  }
  return(sum(foo))
}

## Slide 101

L <- function(theta, x){
  Gamma <- diag(theta[1:2])
  Gamma[1, 2] <- 1 - Gamma[1, 1]
  Gamma[2, 1] <- 1 - Gamma[2, 2] 
  delta <- solve(t(diag(2) - Gamma + 1), c(1, 1))
  lambda <- theta[3:4]
  allprobs <- cbind(dpois(x, lambda[1]), dpois(x, lambda[2]))
  foo <- delta %*% diag(allprobs[1, ])
  for (t in 2:length(x)){
    foo <- foo %*% Gamma %*% diag(allprobs[t, ])
  }
  return(sum(foo))
}

quakes <- read.table("https://tinyurl.com/uu2ukyvj", header = TRUE)
theta <- c(0.8, 0.8, 15, 25)
L(theta, x = quakes$count)
