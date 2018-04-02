# IMPORTANT NOTE:
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression/")

# read in data
data_diabetes=read.csv("./data/diabetes.csv")

# get columns
data_preg=data_diabetes$Pregnancies
data_gluc=data_diabetes$Glucose
data_bp=data_diabetes$BloodPressure
data_st=data_diabetes$SkinThickness
data_ins=data_diabetes$Insulin
data_bmi=data_diabetes$BMI
data_ped=data_diabetes$DiabetesPedigreeFunction
data_age=data_diabetes$Age
data_outcome=data_diabetes$Outcome


##### Fill Holes #####

# calculate means
mean_gluc=mean(data_gluc[which(data_gluc!=0)])
mean_bp=mean(data_bp[which(data_bp!=0)])
mean_st=mean(data_st[which(data_st!=0)])
mean_ins=mean(data_ins[which(data_ins!=0)])
mean_bmi=mean(data_bmi[which(data_bmi!=0)])

# copy data
data_filled=data_diabetes
data_gluc_filled=data_gluc
data_bp_filled=data_bp
data_st_filled=data_st
data_ins_filled=data_ins
data_bmi_filled=data_bmi

# fill holes (zeros) with means
for(i in 1:nrow(data_diabetes))
{
  # glucose
  if(data_gluc_filled[i]==0)
    data_gluc_filled[i]=mean_gluc
  
  # blood pressure
  if(data_bp_filled[i]==0)
    data_bp_filled[i]=mean_bp
  
  # skin thickness
  if(data_st_filled[i]==0)
    data_st_filled[i]=mean_st
  
  # insulin
  if(data_ins_filled[i]==0)
    data_ins_filled[i]=mean_ins
  
  if(data_bmi_filled[i]==0)
    data_bmi_filled[i]=mean_bmi
}

# set filled columns
data_filled$Glucose=data_gluc_filled
data_filled$BloodPressure=data_bp_filled
data_filled$SkinThickness=data_st_filled
data_filled$Insulin=data_ins_filled
data_filled$BMI=data_bmi_filled

# output filled data
write.csv(data_filled, file = "./data/diabetes_filled.csv")

# split data into train and test
require(caTools)
set.seed(516)
data_split=sample.split(data_filled,SplitRatio=3/4)
train=subset(data_filled,data_split==TRUE)
test=subset(data_filled,data_split==FALSE)

# output split data
write.csv(train, file = "./data/diabetes_filled_train.csv")
write.csv(test, file = "./data/diabetes_filled_test.csv")


##### Remove Outliers #####

# calculate upper and lower fences
data_removed=data_filled

# blood Pressure
summary(data_bp_filled)
uf_bp=80+1.5*(80-64)
lf_bp=64-1.5*(80-64)

# skin thickness
summary(data_st_filled)
uf_st=32+1.5*(32-20.54)
lf_st=20.54-1.5*(32-20.54)

# insulin
summary(data_ins_filled)
uf_ins=127.2+1.5*(127.2-79.8)
lf_ins=79.8-1.5*(127.2-79.8)

# BMI
summary(data_bmi_filled)
uf_bmi=36.60+1.5*(36.60-27.50)
lf_bmi=27.50-1.5*(36.60-27.50)

# remove entire row if any of its features are outliers
num_data_removed_rows = nrow(data_removed)
i = 1
while(i <= num_data_removed_rows)
{
  remove_row = 0
  
  # blood pressure
  if(data_removed$BloodPressure[i]<lf_bp | data_removed$BloodPressure[i]>uf_bp)
    data_removed=data_removed[-i,]
    removed_row = 1
  
  # skin thickness
  if(data_removed$SkinThickness[i]<lf_st | data_removed$SkinThickness[i]>uf_st)
    data_removed=data_removed[-i,]
    removed_row = 1
  
  # insulin
  if(data_removed$Insulin[i]<lf_ins | data_removed$Insulin[i]>uf_ins)
    data_removed=data_removed[-i,]
    remove_row = 1
  
  # BMI
  if(data_removed$BMI[i]<lf_bmi | data_removed$BMI[i]>uf_bmi)
    data_removed=data_removed[-i,]
    remove_row = 1
    
  if(remove_row)
    num_data_removed_rows = num_data_removed_rows - 1
    
  i = i + 1
}