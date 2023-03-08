Linear_Regression <- setRefClass("Linear_Regression",
  fields = list(weights_n = "numeric",
    weights = "numeric",
    lr = "numeric",
    bias = "numeric"),
  methods = list(
    
    initialize = function(..., weights_n = numeric()){
        callSuper(...)
        .self$weights <- c(rep(0, weights_n))
    },
    
    weights_training = function(X, y) {
        weights <<- weights + lr*c(solve(t(X)%*%X)%*%t(X)%*%y)
    },
    
    predict = function(X) {
        return (X %*% weights + bias)
    },
    
    calc_accuracy = function(y_pred, y_true) {
	return (sum(y_pred == y_true) / length(y_true))
    }
    
  )
)

