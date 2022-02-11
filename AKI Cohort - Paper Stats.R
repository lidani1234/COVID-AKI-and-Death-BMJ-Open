source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(plotROC)
require(pROC)
require(nricens)

# Abstract
summary(data$Age); sd(data$Age)
sum(status.AKI); mean(status.AKI)
sum(AKI3d); sum(AKI3d)/sum(status.AKI)
sum(status.death); mean(status.death)
sum(Death30d); sum(Death30d)/sum(status.death)

# Descriptive stats
prop.test(c(192, 155), c(696, 3020))

##########################
# Models
##########################

# Variables
Age4565 = as.numeric(Age == "45-65")
Age65 = as.numeric(Age == ">65")
WBC11 = as.numeric(data$WBC > 11)
HGB10 = as.numeric(data$HGB < 10)
CRP100 = as.numeric(data$CRP > 100)
DDimer1000 = as.numeric(data$DDimer >= 1000)
DDimer2000 = as.numeric(data$DDimer >= 2000)

# Models
cox.AKI.simp <- coxph(Surv(time.AKI, status.AKI) ~ Sex + WBC11 + HGB + CRP100 + DDimer1000)
cox.AKI.full <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
                        Diabetes + CVD + HTN + HF + 
                        BMI + Temp + HR+ SBP +
                        WBC + HGB + PLT + CRP + Ferritin + DDimer)
cox.AKI.review <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
                        Diabetes + CVD + HTN + HF + 
                        BMI + Temp + HR+ SBP +
                        WBC + HGB + PLT + CRP + Ferritin + DDimer +
                        Lymphopenia + MV)
cox.death.simp <- coxph(Surv(time.death, status.death) ~ Age + WBC11 + PLT + CRP100 + DDimer2000)
cox.death.full <- coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
                          Diabetes + CVD + HTN + HF + 
                          BMI + Temp + HR+ SBP +
                          WBC + HGB + PLT + CRP + Ferritin + DDimer)
cox.death.review <- coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
                          Diabetes + CVD + HTN + HF + 
                          BMI + Temp + HR+ SBP +
                          WBC + HGB + PLT + CRP + Ferritin + DDimer +
                          Lymphopenia + MV)

# Linear scores
pred.AKI.simp = cox.AKI.simp$linear.predictors
pred.AKI.full = cox.AKI.full$linear.predictors
pred.AKI.review = cox.AKI.review$linear.predictors
pred.death.simp = cox.death.simp$linear.predictors
pred.death.full = cox.death.full$linear.predictors
pred.death.review = cox.death.review$linear.predictors

# Simplified risk scores
score.AKI = as.numeric(HGB=="<10")*2 + as.numeric(HGB=="10-12")*1 +
  + DDimer1000*1 + CRP100*1 + WBC11 + as.numeric(Sex=="Male")*1 
score.death = Age65*3 + Age4565*2 + CRP100*1 + as.numeric(PLT=="<100")*1 + 
  WBC11*1 + DDimer1000*1

##########################
# Harrell's Survival C-Statistic
##########################

# Approximate CI
con.CI = function(x){
  c(x$concordance["concordance"],
    x$concordance["concordance"] - 1.96 * x$concordance["std"],
    x$concordance["concordance"] + 1.96 * x$concordance["std"])
}

round(con.CI(cox.AKI.simp), digits = 3)
round(con.CI(cox.AKI.full), digits = 3)
round(con.CI(cox.death.simp), digits = 3)
round(con.CI(cox.death.full), digits = 3)

# Data frame
dt = data.frame(time.AKI=time.AKI, status.AKI=status.AKI, 
                time.death=time.death, status.death=status.death,
                Age=Age, Sex=as.factor(Sex), Race=Race, 
                Diabetes=Diabetes, CVD=CVD, HTN=HTN, HF=HF, Conditions=Conditions,
                BMI=BMI, Temp=Temp, HR=HR, SBP=SBP, RR=RR,
                WBC=WBC, HGB=HGB, PLT=PLT, CRP=CRP, Ferritin=Ferritin, DDimer=DDimer, IL6=IL6, 
                Age65=Age65, WBC11=WBC11, HGB10=HGB10, CRP100=CRP100,
                DDimer1000=DDimer1000, DDimer2000=DDimer2000)

# Bootstrap CI check
con = numeric(length = 1000); con[1] = cox.AKI.simp$concordance["concordance"]
n = nrow(data)
for(i in 2:1000){
  ind = sample(1:n, n, replace = TRUE)
  dt.sub = dt[ind,]
  cox.AKI <- coxph(Surv(time.AKI, status.AKI) ~ Sex + HGB + CRP100 + DDimer1000, data=dt.sub)
  con[i] = cox.AKI$concordance["concordance"]
}
cox.AKI.simp$concordance["concordance"]; quantile(con, c(0.025, 0.975))

##########################
# AKI 3d and Death 30d AUC results
##########################

# Binary outcomes
AKI3d = as.numeric(status.AKI == 1 & time.AKI <= 3)
Death30d = as.numeric(status.death == 1 & time.death <= 30)

# Categorical AUC's
auc(roc(AKI3d, score.AKI)); ci(roc(AKI3d, score.AKI))
auc(roc(AKI3d, pred.AKI.simp)); ci(roc(AKI3d, pred.AKI.simp))
auc(roc(AKI3d, pred.AKI.full)); ci(roc(AKI3d, pred.AKI.full))
auc(roc(AKI3d, pred.AKI.review)); ci(roc(AKI3d, pred.AKI.review))

auc(roc(Death30d, score.death)); ci(roc(Death30d, score.death))
auc(roc(Death30d, pred.death.simp)); ci(roc(Death30d, pred.death.simp))
auc(roc(Death30d, pred.death.full)); ci(roc(Death30d, pred.death.full))
auc(roc(Death30d, pred.death.review)); ci(roc(Death30d, pred.death.review))

##########################
# AKI 3d and Death 30d NRI results
##########################

nribin(event = AKI3d, p.std = pred.AKI.simp, p.new = pred.AKI.full, updown="category", cut=0.01)
nribin(event = Death30d, p.std = pred.death.simp, p.new = pred.death.full, updown="category", cut=0.01)
