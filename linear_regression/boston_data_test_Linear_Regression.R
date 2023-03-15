source("Linear_Regression.R")
library(MASS)
library(ggcorrplot)

dim(Boston)
plot(medv~lstat, Boston)
plot(~ medv + ptratio + black + lstat + dis + rm + crim, data = Boston, main = "Boston Data")

Linear_R <- Linear_Regression$new(weights_n = 13, bias = 0, lr = 1)

sample <- sample(c(TRUE, FALSE), nrow(Boston), replace=TRUE, prob=c(0.7,0.3))

X <- within(data.frame(Boston), rm("medv"))
y <- Boston[,"medv"]
X_train  <- as.matrix(X[sample,])
y_train <- y[sample]

X_test  <- as.matrix(X[!sample,])
y_test <- y[!sample]


Linear_R$weights_training(X_train, y_train)
output <- Linear_R$predict(X_test)

df <- data.frame(output, y_test)
df

Linear_R$calc_loss(X_test, y_test)
