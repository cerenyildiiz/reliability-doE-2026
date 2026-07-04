# install.packages(c("survival", "tidyverse"))

library(survival)
library(tidyverse)

set.seed(123)

B <- 1000
n <- 36

scale_true <- 3000
censor_time <- 4000

shapes <- c(1.085, 1.1, 1.3, 1.85, 2, 3)

results <- data.frame()

for (s in shapes) {
  
  selected_AICc <- c()
  
  for (b in 1:B) {
    
    # Veri Weibull dağılımı kullanılarak üretilmiştir.
    time_true <- rweibull(n, shape = s, scale = scale_true)
    
    # Tip I sağdan sansürlü veri söz konusudur.
    time_obs <- pmin(time_true, censor_time)
    status <- ifelse(time_true <= censor_time, 1, 0)
    
    data <- data.frame(time = time_obs, status = status)
    # Null (boş) model kurulmuştur.
    weibull_model <- survreg(Surv(time, status) ~ 1,
                             data = data,
                             dist = "weibull")
    
    exponential_model <- survreg(Surv(time, status) ~ 1,
                                 data = data,
                                 dist = "exponential")
    
    lognormal_model <- survreg(Surv(time, status) ~ 1,
                               data = data,
                               dist = "lognormal")
    
    fits <- list(
      Exponential = exponential_model,
      Lognormal   = lognormal_model,
      Weibull     = weibull_model
    )
    
    AICc_values <- sapply(names(fits), function(model_name) {
      
      fit <- fits[[model_name]]
      
      if (model_name == "Exponential") {
        k <- 1
      } else {
        k <- 2
      }
      # Modelde kullanılan üstel dağılım tek parametreli olduğundan  iki parametreli üstel dağılımla karışmaması amacıyla bu adım uygulanmıştır
    aic <- AIC(fit)
      aic + (2 * k * (k + 1)) / (n - k - 1)
    })
    
    selected_AICc[b] <- names(which.min(AICc_values))
  }
  
  tmp <- prop.table(table(selected_AICc))
  
  results <- rbind(
    results,
    data.frame(
      Shape = s,
      Exponential = ifelse("Exponential" %in% names(tmp), tmp["Exponential"], 0),
      Lognormal   = ifelse("Lognormal" %in% names(tmp), tmp["Lognormal"], 0),
      Weibull     = ifelse("Weibull" %in% names(tmp), tmp["Weibull"], 0)
    )
  )
}

results
