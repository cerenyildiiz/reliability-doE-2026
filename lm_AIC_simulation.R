# AIC ile model seçimi sonrası klasik p-değerlerinin yanıltıcı olabileceğini
# gösteren basit bir simülasyon 
# install.packages("MASS")
set.seed(123)

library(MASS)


data <- data.frame(matrix(runif(20000), ncol = 100))

dat$y <- rnorm(200)


full_model <- lm(y ~ ., data = dat)


summary(full_model)


# selected_model <- stepAIC(full_model, trace = FALSE)
selected_model <- stepAIC(full_model)



summary(selected_model)
