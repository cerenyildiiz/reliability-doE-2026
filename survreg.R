


library(survival)

x <- rweibull(1000, shape = 2, scale = 5)

fit <- survreg(Surv(x) ~ 1, dist = "weibull")

shape <- 1 / fit$scale
scale <- exp(coef(fit))

u <- runif(1000)
x2 <- scale * (-log(1 - u))^(1 / shape)

x3 <- rweibull(1000, shape = shape, scale = 5)

plot(sort(x), sort(x2))
abline(a = 0, b = 1)
