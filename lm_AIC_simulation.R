# AIC ile model seçimi sonrası klasik p-değerlerinin yanıltıcı olabileceğini
# gösteren basit bir simülasyon 
# install.packages("MASS")
set.seed(123)

library(MASS)


data <- data.frame(matrix(runif(20000), ncol = 100))

data$y <- rnorm(200)


full_model <- lm(y ~ ., data = data)


summary(full_model)


# selected_model <- stepAIC(full_model, trace = FALSE)
selected_model <- stepAIC(full_model)



summary(selected_model)

--------------------------------------------
set.seed(123)

library(MASS)

n <- 200
p <- 100

data <- data.frame(matrix(runif(n * p), ncol = p))
data$y <- rnorm(n)

full_model <- lm(y ~ ., data = data)

summary(full_model)

selected_model <- stepAIC(full_model)

summary(selected_model)
