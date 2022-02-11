source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting 1.R")

require("glmnet")
require(My.stepwise)

# Covariates from regression analyses
cov = data.frame(Age=Age, Sex=as.factor(Sex), Race=Race, 
                 Diabetes=Diabetes, CVD=CVD, HTN=HTN, HF=HF, Conditions=Conditions,
                 BMI=BMI, Temp=Temp, HR=HR, SBP=SBP, RR=RR,
                 WBC=WBC, HGB=HGB, PLT=PLT, CRP=CRP, Ferritin=Ferritin, DDimer=DDimer, IL6=IL6)

# Make categorical variables multiple indicator variables
X = makeX(cov)
colnames(X)
ind = c(2:3, 5, 7:10, 20:22, 
        24:25, 27:28, 30:31, 33:34, 
        39:40, 42:43, 45, 47:48, 50:51, 53:54)
colnames(X)[ind]
cov1 = data.frame(X[,ind])

# Additional variables for variable selection
cov1$Age45plus = cov1$Age45to65 + cov1$Agegreater65
cov1$Conditions1plus = cov1$Conditions1 + cov1$Conditions2 + cov1$Conditions3plus
cov1$Conditions2plus = cov1$Conditions2 + cov1$Conditions3plus
cov1$BMI25plus = cov1$BMI25to30 + cov1$BMIgreater30
cov1$HGBless12 = as.numeric(data$HGB <= 12)
cov1$CRP50plus = cov1$CRP50to100 + cov1$CRPgreater100
cov1$Ferritin250plus = cov1$Ferritin250to1000 + cov1$Ferritingreater1000
cov1$DDimer1000plus = cov1$DDimer1000to2000 + cov1$DDimergreater2000

# Data frame for variable selection
dt = data.frame(time.AKI=time.AKI, status.AKI=status.AKI, 
                time.death=time.death, status.death=status.death)
dt = cbind(dt, cov1)

# Variable selection
my.variable.list <- colnames(dt)[-(1:4)]
My.stepwise.coxph(Time = "time.AKI", Status = "status.AKI", variable.list = my.variable.list,
                  data = dt, sle = 0.15, sls = 0.15)

My.stepwise.coxph(Time = "time.death", Status = "status.death", variable.list = my.variable.list,
                  data = dt, sle = 0.15, sls = 0.15)
