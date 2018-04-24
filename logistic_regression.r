# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# load data
train_data=read.csv("./data/diabetes_filled_train.csv")
test_data=read.csv("./data/diabetes_filled_test.csv")

# remove extra column
train_data$X = NULL
test_data$X = NULL

# mean normalization?

# split features and outcomes
X = as.matrix(train_data[,1:8])
X = cbind(X, 1)
Y = as.matrix(train_data$Outcome)

# sigmoid function
sigmoid = function(x) {
  return(1/(1+exp(-x)))
}

# linear hypothesis function
linear_hyp = function(weights, data) {
  return(sigmoid(as.matrix(data)%*%weights))
}

# quadratic hypothesis function
quadratic_hyp = function(weights, data) {
  return(sigmoid(as.matrix(data)%*%weights))
}

# mean square error cost function
mean_squared_cost = function(weights, data, hyp_funct) {
  cost = (1/(2*nrow(data)))*sum((hyp_funct(weights, data) - Y)^2)
  return(cost)
}

# logistic cost function
logistic_cost = function(weights, data, hyp_funct) {
  cost = (1/nrow(data))*sum((-Y)*log(hyp_funct(weights, data)) - (1-Y)*log(1-hyp_funct(weights, data)))
  return(cost)
}


# initialize vars for gradient descent
mean_squared_weights = as.matrix(rep(0.00001,ncol(X)))
logistic_weights = as.matrix(rep(0,ncol(X)))
learning_rate = 0.1
epochs = 100

mean_squared_cost(mean_squared_weights, X, linear_hyp)
logistic_cost(logistic_weights, X, linear_hyp)

# gradient descent
gradient_descent = function() {
  # gradient descent
  #   take in alpha (learning rate)
  #   take in weights and bias
  #   compute gradient
  #   update weights and bias 
}

# get accuracy from test data