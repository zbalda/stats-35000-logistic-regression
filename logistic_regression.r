# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# read in data
diabetes_data=read.csv("./data/diabetes.csv")
diabetes_filled_data=read.csv("./data/diabetes_filled.csv")

diabetes_filled_data$X = NULL

# load in training data
# load in testing data
# remove number column(s)

# split train and test datasets into two
#   1. features X
#   2. results Y

# mean normalization?

# define sigmoid function

# define hypothesis functions
#   take in n weights
#   take in bias
#   take in n feature values
#   calculate dot product of weights and feature values
#   add bias
#   sigmoid
#   return value

# define cost functions
#   take in n weights
#   take in bias
#   calculate cost from using train features and train results
#   return cost

# initialize k weights for k features
# initialize bias

# gradient descent
#   take in alpha (learning rate)
#   take in weights and bias
#   compute gradient
#   update weights and bias

# get accuracy from test data

# sigmoid function
sigmoid = function(x) {
  return(1/(1+exp(-x)))
}

# linear hypothesis function
linear_hyp = function() {
  
}

# quadratic hypothesis function
quadratic_hyp = function() {
  
}

# mean square error cost function
mean_squared_cost = function() {
  
}

# logistic cost function
logistic_cost = function() {
  
}

# gradient descent
gradient_descent = function(cost_function, ) {
  
}