# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# read in data
diabetes_data=read.csv("./data/diabetes.csv")
diabetes_filled_data=read.csv("./data/diabetes_filled.csv")

# Linear Regression Example
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

# Multiple Box Plots
require(reshape2)
new_diabetes_filled_data = diabetes_filled_data
new_diabetes_filled_data$X = NULL
new_diabetes_filled_data$Insulin = NULL
new_diabetes_filled_data$DiabetesPedigreeFunction = NULL
new_diabetes_filled_data.m = melt(new_diabetes_filled_data, id.var="Outcome")
require(ggplot2)
ggplot(data=new_diabetes_filled_data.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Outcome))


# Overlaid density plots with semi-transparent fill
require(ggplot2)

diabetes=data.frame(diabetes_data[diabetes_data$Outcome == 1,])
no_diabetes=data.frame(diabetes_data[diabetes_data$Outcome == 0,])

diabetes_feature=data.frame(feature = diabetes$BMI)
no_diabetes_feature=data.frame(feature = no_diabetes$BMI)

diabetes_feature$Outcome = 'Diabetes'
no_diabetes_feature$Outcome = 'No Diabetes'

outcome = rbind(diabetes_feature, no_diabetes_feature)

ggplot(outcome, aes(feature, fill = Outcome)) + geom_density(alpha = 0.3)