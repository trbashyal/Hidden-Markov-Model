## Slide 71

n <- 50
x <- s <- rep(NA, times = n)
Gamma <- matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2)
delta <- c(0.5, 0.5)
mu <- c(5, 14)
sigma <- c(2, 3)

s[1] <- sample(x = 1:2, size = 1, prob = delta)
x[1] <- rnorm(n = 1, mu[s[1]], sigma[s[1]])

for (t in 2:50){
  s[t] <- sample(x = 1:2, size = 1, prob = Gamma[s[t - 1], ])
  x[t] <- rnorm(n = 1, mu[s[t]], sigma[s[t]])
}
