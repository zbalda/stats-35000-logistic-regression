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