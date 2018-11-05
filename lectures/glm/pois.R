
beta0 <- -2
beta1 <- 0.1
x <- c(1, 2, 1, 1, 2, 3, 3, 5, 3, 5, 9, 3, 5, 3, 10, 11, 120, 100, 80, 79)
n <- length(x)

lambda <- exp(beta0 + beta1*x)
set.seed(340)
y <- rpois(n, lambda)
y

hist(y)

summary(glm(y ~ x, poisson))

