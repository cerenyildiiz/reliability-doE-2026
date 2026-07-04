library(survival)

set.seed(123)

B <- 1000
n <- 36

scale_true <- 3000
censor_time <- 4000

shapes <- c(1.085, 1.1, 1.3, 1.85, 2, 3)

results <- data.frame()

safe_survreg <- function(formula, data, dist) {
  warned <- FALSE
  fit <- withCallingHandlers(
    tryCatch(survreg(formula, data = data, dist = dist), error = function(e) NULL),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warned = warned)
}

for (s in shapes) {

  selected_AICc <- c()
  censor_rate <- c()
  n_fit_errors <- 0
  n_fit_warns <- 0

  for (b in 1:B) {

    time_true <- rweibull(n, shape = s, scale = scale_true)

    time_obs <- pmin(time_true, censor_time)
    status <- ifelse(time_true <= censor_time, 1, 0)

    censor_rate[b] <- mean(status == 0)

    data <- data.frame(time = time_obs, status = status)

    weibull_model <- safe_survreg(Surv(time, status) ~ 1,
                                   data = data,
                                   dist = "weibull")

    exponential_model <- safe_survreg(Surv(time, status) ~ 1,
                                       data = data,
                                       dist = "exponential")

    lognormal_model <- safe_survreg(Surv(time, status) ~ 1,
                                     data = data,
                                     dist = "lognormal")

    fits <- list(
      Exponential = exponential_model,
      Lognormal   = lognormal_model,
      Weibull     = weibull_model
    )

    if (any(sapply(fits, function(f) is.null(f$fit)))) {
      n_fit_errors <- n_fit_errors + 1
    }

    if (any(sapply(fits, function(f) f$warned))) {
      n_fit_warns <- n_fit_warns + 1
    }

    AICc_values <- sapply(names(fits), function(model_name) {

      fit <- fits[[model_name]]$fit

      if (is.null(fit)) {
        return(NA)
      }

      k <- attr(logLik(fit), "df")
      aic <- AIC(fit)
      aic + (2 * k * (k + 1)) / (n - k - 1)
    })

    if (all(is.na(AICc_values))) {
      selected_AICc[b] <- NA
    } else {
      selected_AICc[b] <- names(which.min(AICc_values))
    }
  }

  valid <- !is.na(selected_AICc)
  tmp <- prop.table(table(selected_AICc[valid]))

  results <- rbind(
    results,
    data.frame(
      Shape = s,
      Exponential = ifelse("Exponential" %in% names(tmp), tmp["Exponential"], 0),
      Lognormal   = ifelse("Lognormal" %in% names(tmp), tmp["Lognormal"], 0),
      Weibull     = ifelse("Weibull" %in% names(tmp), tmp["Weibull"], 0),
      Mean_Censor_Rate = mean(censor_rate),
      N_Fit_Warnings   = n_fit_warns,
      N_Fit_Errors     = n_fit_errors,
      N_Valid_Reps     = sum(valid)
    )
  )
}

results
