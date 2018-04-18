# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression")

# read in data
diabetes_data=read.csv("./data/diabetes.csv")
diabetes_filled_data=read.csv("./data/diabetes_filled.csv")

# pregnancies - bar plot
pregnancy_counts=table(diabetes_data$Outcome, diabetes_data$Pregnancies)
barplot(pregnancy_counts,
        main="Bar Plot for Pregnancies",
        xlab="Pregnancies",
        ylab="Count",
        border="black",
        col=c("dodgerblue3","firebrick3"),
        ylim=c(0,140),
        legend=c("No Diabetes", "Diabetes"),
        las=1)

# glucose - histogram
hist(diabetes_data$Glucose,
     main="Histogram for Glucose Levels",
     xlab="Glucose Level",
     ylab="Frequency",
     xlim=c(46,200),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# blood pressure - histogram
hist(diabetes_filled_data$BloodPressure,
     main="Histogram for Blood Pressure",
     xlab="Blood Pressure",
     ylab="Frequency",
     #xlim=c(20,200),
     #ylim=c(0,250),
     border="black",
     col="dodgerblue3",
     las=1)

# skin thickness - histogram
hist(diabetes_filled_data$SkinThickness,
     main="Histogram for Skin Thickness",
     xlab="Skin Thickness",
     ylab="Frequency",
     xlim=c(0,70),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# insulin - histogram
hist(diabetes_filled_data$Insulin,
     main="Histogram for Insulin Levels",
     xlab="Insulin Level",
     ylab="Frequency",
     xlim=c(0,600),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# BMI - histogram
hist(diabetes_data$BMI,
     main="Histogram for BMI",
     xlab="BMI",
     ylab="Frequency",
     xlim=c(15,60),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# Diabetes Pedigree Function - histogram
hist(diabetes_data$DiabetesPedigreeFunction,
     main="Histogram for Diabetes Pedigree Function",
     xlab="Diabetes Pedigree Function",
     ylab="Frequency",
     xlim=c(0,2),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# Age - histogram
hist(diabetes_filled_data$Age,
     main="Histogram for Age",
     xlab="Age",
     ylab="Frequency",
     #xlim=c(0,2),
     #ylim=c(0,120),
     border="black",
     col="dodgerblue3",
     las=1)

# Overlaid histograms
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

require(reshape2)
new_diabetes_filled_data = diabetes_filled_data
new_diabetes_filled_data$X = NULL
new_diabetes_filled_data$Insulin = NULL
new_diabetes_filled_data$DiabetesPedigreeFunction = NULL
new_diabetes_filled_data.m = melt(new_diabetes_filled_data, id.var="Outcome")
require(ggplot2)
ggplot(data=new_diabetes_filled_data.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Outcome))


# Overlaid histograms - Pregnancies
table=table(diabetes_filled_data$BMI, diabetes_filled_data$Outcome)
x1 = data.frame(table[,"0"])
x2 = data.frame(table[,"1"])

brs=11
hist(x1$table....0..,
     main="Histogram of Pregnancies",
     xlab="Pregnancy Count",
     breaks=brs,
     col=rgb(0,0,1,0.5))

hist(x2$table....1.., 
     breaks=brs,
     col=rgb(1,0,0,0.5), 
     add=T)
box()


# barplot for Glucose
barvar=table(diabetes_filled_data$Outcome, diabetes_filled_data$Glucose)
barplot(barvar,
        main="Bar Plot for Glucose",
        xlab="Glucose",
        ylab="Count",
        border="black",
        col=c("dodgerblue3","firebrick3"),
        legend=c("No Diabetes", "Diabetes"),
        las=1)

# barplot for Blood Pressure
barvar=table(diabetes_filled_data$Outcome, diabetes_filled_data$BloodPressure)
barplot(barvar,
        main="Bar Plot for Blood Pressure",
        xlab="Blood Pressure",
        ylab="Count",
        border="black",
        col=c("dodgerblue3","firebrick3"),
        legend=c("No Diabetes", "Diabetes"),
        las=1)


# barplot for BMI
barvar=table(diabetes_data$Outcome, diabetes_data$BMI)
barplot(barvar,
        main="Bar Plot for BMI",
        xlab="BMI",
        ylab="Count",
        border="black",
        col=c("dodgerblue3","firebrick3"),
        legend=c("No Diabetes", "Diabetes"),
        las=1)

# barplot for Age
barvar=table(diabetes_data$Outcome, diabetes_data$Age)
barplot(barvar,
        main="Bar Plot for Age",
        xlab="Age",
        ylab="Count",
        border="black",
        col=c("dodgerblue3","firebrick3"),
        legend=c("No Diabetes", "Diabetes"),
        las=1)

require(ggplot2)

diabetes = data.frame(bmi = rnorm(100000, 6, 2))
no_diabetes = data.frame(bmi = rnorm(50000, 7, 2.5))

diabetes$outcome = 'diabetes'
no_diabetes$outcome = 'no_diabetes'

outcome_bmi = rbind(diabetes, no_diabetes)

ggplot(outcome_bmi, aes(bmi, fill = outcome)) + geom_density(alpha = 0.3)