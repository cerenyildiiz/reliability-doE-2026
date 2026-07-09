 # install.packages(c("survival", "AICcmodavg"))
library(survival)
library(AICcmodavg)

# Modeller
model_weibull <- survreg(
  Surv(Lifetime, Cens) ~ A + B + C + D + E + F + G,
  data = data,
  dist = "weibull"
)

model_lognormal <- survreg(
  Surv(Lifetime, Cens) ~ A + B + C + D + E + F + G,
  data = data,
  dist = "lognormal"
)

model_exp <- survreg(
  Surv(Lifetime, Cens) ~ A + B + C + D + E + F + G,
  data = data,
  dist = "exponential"
)


cand.models <- list(
  Exponential = model_exp,
  Weibull     = model_weibull,
  Lognormal   = model_lognormal
)

# AICc tablosu
aictab(
  cand.set = cand.models,
  modnames = names(cand.models),
  second.ord = TRUE
)







# References: 1. https://www.rdocumentation.org/packages/AICcmodavg/versions/1.0/topics/aictab
