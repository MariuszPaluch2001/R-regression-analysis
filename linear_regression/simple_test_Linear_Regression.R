source("Linear_Regression.R")

f <- (function(x) 0.5 * x + runif(length(x), min=0, max=5)) # Linear function with bias noise
X <- runif(100, min=0, max=20)
dim(X) <- c(100,1)
y <- f(X)
plot(X,y, col="green")

Linear_R <- Linear_Regression$new(weights_n = 1, bias = 1, lr = 1)

Linear_R$weights_training(X,y)

X <- 0:20
dim(X) <- c(21,1)
y_test = Linear_R$predict(X)
lines(X, y_test, 'l', col="red")
