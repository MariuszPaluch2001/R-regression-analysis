source("Linear_Regression.R")
library(datasets)
library(ggcorrplot)

dim(longley)
longley

pairs(longley)

cors <- cor(longley, use = "pairwise.complete.obs")
ggcorrplot(cors)

Linear_R <- Linear_Regression$new(weights_n = 6, bias = 0, lr = 1)

y <- longley$Employed
X <- subset(longley, select = -c(Employed))
X <- as.matrix(X)
X_test = X[1,]
y_test = y[1]
X = X[2:16,]
y = y[2:16]

Linear_R$weights_training(X,y)

output <- Linear_R$predict(X_test)

print("Sample:")
X_test
sprintf("Correct: %f", y_test[1])
sprintf("Output: %f", output[1])
