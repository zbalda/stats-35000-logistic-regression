# read in data
#   set working directory in the console first
#   e.g. setwd("/home/.../.../stats-35000-logistic-regression/")
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
mean_gluc=mean(data_gluc)
mean_bp=mean(data_bp)
mean_st=mean(data_st)
mean_ins=mean(data_ins)
mean_bmi=mean(data_bmi)
data_filled=data_diabetes

# copy data columns
data_gluc_filled=data_gluc
data_bp_filled=data_bp
data_st_filled=data_st
data_ins_filled=data_ins
data_bmi_filled=data_bmi

# fill holes (zeros) with means
for(i in c(1:768))
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