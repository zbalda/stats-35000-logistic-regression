#Reading in data
data_diabetes=read.csv("./diabetes.csv")
data_preg=data_diabetes$Pregnancies
data_gluc=data_diabetes$Glucose
data_bp=data_diabetes$BloodPressure
data_st=data_diabetes$SkinThickness
data_ins=data_diabetes$Insulin
data_bmi=data_diabetes$BMI
data_ped=data_diabetes$DiabetesPedigreeFunction
data_age=data_diabetes$Age
data_outcome=data_diabetes$Outcome
#Filing in holes with mean prior to removing outliers (Holes only exist for bp, st, ins, and bmi)
mean_bp=mean(data_bp)
mean_st=mean(data_st)
mean_ins=mean(data_ins)
mean_bmi=mean(data_bmi)
data_filled=data_diabetes
#Blood Pressure
data_bp_filled=data_bp
for(i in c(1:768))
{
  if(data_bp_filled[i]==0)
    data_bp_filled[i]=mean_bp
}
data_filled$BloodPressure=data_bp_filled
#Skin Thickness
data_st_filled=data_st
for(i in c(1:768))
{
  if(data_st_filled[i]==0)
    data_st_filled[i]=mean_st
}
data_filled$SkinThickness=data_st_filled
#Insulin
data_ins_filled=data_ins
for(i in c(1:768))
{
  if(data_ins_filled[i]==0)
    data_ins_filled[i]=mean_ins
}
data_filled$Insulin=data_ins_filled
#BMI
data_bmi_filled=data_bmi
for(i in c(1:768))
{
  if(data_bmi_filled[i]==0)
    data_bmi_filled[i]=mean_bmi
}
data_filled$BMI=data_bmi_filled
#Calculating upper and lower fence values, to remove outliers
data_removed=data_filled
#Blood Pressure
summary(data_bp_filled)
uf_bp=80+1.5*(80-64)
lf_bp=64-1.5*(80-64)
for(i in c(1:768))
{
  if(data_removed$BloodPressure[i]<lf_bp | data_removed$BloodPressure[i]>uf_bp)
    data_removed=data_removed[-i,]
}
#Skin Thickness
summary(data_st_filled)
uf_st=32+1.5*(32-20.54)
lf_st=20.54-1.5*(32-20.54)
for(i in c(1:768))
{
  if(data_removed$SkinThickness[i]<lf_st | data_removed$SkinThickness[i]>uf_st)
    data_removed=data_removed[-i,]
}
#Insulin
summary(data_ins_filled)
uf_ins=127.2+1.5*(127.2-79.8)
lf_ins=79.8-1.5*(127.2-79.8)
for(i in c(1:768))
{
  if(data_removed$Insulin[i]<lf_ins | data_removed$Insulin[i]>uf_ins)
    data_removed=data_removed[-i,]
}
#BMI
summary(data_bmi_filled)
uf_bmi=36.60+1.5*(36.60-27.50)
lf_bmi=27.50-1.5*(36.60-27.50)
for(i in c(1:768))
{
  if(data_removed$BMI[i]<lf_bmi | data_removed$BMI[i]>uf_bmi)
    data_removed=data_removed[-i,]
}
#Not filling holes, but just removing outliers from original dataset
data_diabetes_removed=data_diabetes
#Blood Pressure
summary(data_diabetes_removed$BloodPressure)
uf_bpo=80+1.5*(80-62)
lf_bpo=62-1.5*(80-62)
for(i in c(1:768))
{
  if(data_diabetes_removed$BloodPressure[i]<lf_bpo | data_diabetes_removed$BloodPressure[i]>uf_bpo)
    data_diabetes_removed=data_diabetes_removed[-i,]
}
#Skin Thickness
summary(data_diabetes_removed$SkinThickness)
uf_sto=32+1.5*(32-23)
lf_sto=23-1.5*(32-23)
for(i in c(1:768))
{
  if(data_diabetes_removed$SkinThickness[i]<lf_sto | data_diabetes_removed$SkinThickness[i]>uf_sto)
    data_diabetes_removed=data_diabetes_removed[-i,]
}
#Insulin
summary(data_diabetes_removed$Insulin)
uf_inso=127.2+1.5*(127.2-0)
lf_inso=0-1.5*(127.2-0)
for(i in c(1:768))
{
  if(data_diabetes_removed$Insulin[i]<lf_inso | data_diabetes_removed$Insulin[i]>uf_inso)
    data_diabetes_removed=data_diabetes_removed[-i,]
}
#BMI
summary(data_diabetes_removed$BMI)
uf_bmio=36.60+1.5*(36.60-27.30)
lf_bmio=27.30-1.5*(36.60-27.30)
for(i in c(1:768))
{
  if(data_diabetes_removed$BMI[i]<lf_bmio | data_diabetes_removed$BMI[i]>uf_bmio)
    data_diabetes_removed=data_diabetes_removed[-i,]
}