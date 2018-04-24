# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# read in data
diabetes_data=read.csv("./data/diabetes.csv")
diabetes_filled_data=read.csv("./data/diabetes_filled.csv")


### linear regression example
pregnancy_count=as.data.frame.matrix(table(diabetes_data$Outcome, diabetes_data$Pregnancies))
plot = ggplot(pregnancy_count, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
plot

plot(diabetes_filled_data$Glucose, 
     diabetes_filled_data$BloodPressure,
     main="Linear Regression",
     xlab="Glucose",
     ylab="Blood Pressure")
abline(lm(diabetes_filled_data$BloodPressure~diabetes_filled_data$Glucose), col="blue")


### logistic regression example
no_zeros = diabetes_data[diabetes_data$BloodPressure != 0,]
no_zeros = no_zeros[no_zeros$Glucose != 0,]
plot(no_zeros$BloodPressure,no_zeros$Glucose,col=as.factor(no_zeros$Outcome),
     xlab="Blood Pressure",
     ylab="Glucose")
legend(1, 1, legend=c("X","Y"), col=c("red","black"))


### multiple box plots
require(reshape2)
new_diabetes_filled_data = diabetes_filled_data
new_diabetes_filled_data$X = NULL
new_diabetes_filled_data$Insulin = NULL
new_diabetes_filled_data$DiabetesPedigreeFunction = NULL
new_diabetes_filled_data.m = melt(new_diabetes_filled_data, id.var="Outcome")
require(ggplot2)
ggplot(data=new_diabetes_filled_data.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Outcome))


### overlaid density plots with semi-transparent fill
require(ggplot2)

# split data by outcome
diabetes=data.frame(diabetes_data[diabetes_data$Outcome == 1,])
no_diabetes=data.frame(diabetes_data[diabetes_data$Outcome == 0,])

# get specific feature of split data
diabetes_feature=data.frame(Age = diabetes$Age)
no_diabetes_feature=data.frame(Age = no_diabetes$Age)

# remove any 0s for Glucose, BloodPressure, SkinThickness, Insulin, and BMI
diabetes_feature=data.frame(Age = diabetes_feature[diabetes_feature$Age != 0,])
no_diabetes_feature=data.frame(Age = no_diabetes_feature[no_diabetes_feature$Age != 0,])

# add outcome label
diabetes_feature$Outcome = 'Diabetes'
no_diabetes_feature$Outcome = 'No Diabetes'

# combine split data
outcome = rbind(diabetes_feature, no_diabetes_feature)

# graph with transparent density plot
ggplot(outcome, 
       aes(Age, fill = Outcome)) + geom_density(alpha = 0.3)