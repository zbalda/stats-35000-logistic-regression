# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# load data
train_data=read.csv("./data/diabetes_filled_train.csv")
test_data=read.csv("./data/diabetes_filled_test.csv")

# remove extra columns
train_data$X = NULL
test_data$X = NULL

# split training data
X_train = as.matrix(train_data[,1:8])
X_train = cbind(X_train, 1)
Y_train = as.matrix(train_data$Outcome)

# split testing data
X_test = as.matrix(test_data[,1:8])
X_test = cbind(X_test, 1)
Y_test = as.matrix(test_data$Outcome)

# mean normalize?
# scale(data, scale=FALSE)

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
mean_squared_cost = function(weights, data, outcome, hyp_funct) {
  cost = (1/(2*nrow(data)))*sum((hyp_funct(weights, data) - outcome)^2)
  return(cost)
}

# logistic cost function
logistic_cost = function(weights, data, outcome, hyp_funct) {
  cost = (1/nrow(data))*sum((-outcome)*log(hyp_funct(weights, data)) - (1-outcome)*log(1-hyp_funct(weights, data)))
  return(cost)
}

# initialize weights
mean_squared_weights = as.matrix(rep(0,ncol(X_train)))
logistic_weights = as.matrix(rep(0,ncol(X_train)))

# optimize parameters
optim_m = optim(par=mean_squared_weights, fn=mean_squared_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp)
optim_l = optim(par=logistic_weights, fn=logistic_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp)

# accuracy function
accuracy = function(weights, data, outcome, hyp_funct){
  hyp = hyp_funct(weights, data)
  diff = abs(hyp-outcome)
  correct = subset(diff, diff[,1] <= 0.5)
  return(nrow(correct)/nrow(data))
}

# get accuracies
print(accuracy(optim_m$par, X_test, Y_test, linear_hyp))
print(accuracy(optim_l$par, X_test, Y_test, linear_hyp))




