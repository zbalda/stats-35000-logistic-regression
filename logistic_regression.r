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
# optim_m = optim(par=mean_squared_weights, fn=mean_squared_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.6784314
# optim_l = optim(par=logistic_weights, fn=logistic_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.6745098

optim_m = optim(par=mean_squared_weights, method="BFGS", fn=mean_squared_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.7686275
optim_l = optim(par=logistic_weights, method="BFGS", fn=logistic_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.7882353

# optim_m = optim(par=mean_squared_weights, method="CG", fn=mean_squared_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.6862745
# optim_l = optim(par=logistic_weights, method="CG", fn=logistic_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.6980392

# optim_m = optim(par=mean_squared_weights, method="L-BFGS-B", fn=mean_squared_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.7058824
# optim_l = optim(par=logistic_weights, method="L-BFGS-B", fn=logistic_cost, data=X_train, outcome=Y_train, hyp_funct=linear_hyp) # 0.6745098

# accuracy function
accuracy = function(weights, data, outcome, hyp_funct){
  hyp = hyp_funct(weights, data)
  diff = abs(hyp-outcome)
  correct = subset(diff, diff[,1] <= 0.5)
  return(nrow(correct)/nrow(data))
}

# get accuracies
print(accuracy(optim_m$par, X_test, Y_test, linear_hyp))Survived
print(accuracy(optim_l$par, X_test, Y_test, linear_hyp))

# train on all data
X = rbind(X_train, X_test)
Y = rbind(Y_train, Y_test)
weights = as.matrix(rep(0,ncol(X)))
optim = optim(par=weights, method="BFGS", fn=logistic_cost, data=X, outcome=Y, hyp_funct=linear_hyp)

# fit data with glm for interpretation
data = rbind(train_data, test_data)
model <- glm(Outcome ~.,family=binomial(link='logit'),data=data)
summary(model)
anova(model, test="Chisq")

# get accuracy of glm optimization
model <- glm(Outcome ~.,family=binomial(link='logit'),data=train_data)



