source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(pROC)

######################################
# Data Frame
######################################

# Variables
Age4565 = as.numeric(Age == "45-65")
Age65 = as.numeric(Age == ">65")
Male = as.numeric(Sex == "Male")
WBC11 = as.numeric(data$WBC > 11)
HGB10 = as.numeric(HGB == "<10")
HGB1012 = as.numeric(HGB == "10-12")
PLT100 = as.numeric(PLT == "<100")
CRP100 = as.numeric(data$CRP > 100)
DDimer1000 = as.numeric(data$DDimer >= 1000)
DDimer2000 = as.numeric(data$DDimer >= 2000)

# Data frame
dt = data.frame(time.AKI=time.AKI, status.AKI=status.AKI, 
                time.death=time.death, status.death=status.death,
                Age4565=Age4565, Age65=Age65, Male=Male,
                WBC11=WBC11, HGB10=HGB10, HGB1012=HGB1012, PLT100=PLT100,
                CRP100=CRP100, DDimer1000=DDimer1000, DDimer2000=DDimer2000)

######################################
# k-fold cross validation
######################################

set.seed(83471304)
n = nrow(dt)
k = 10
step = floor(n / k)
ind = sample(n, n, replace=FALSE)
AUC.AKI = AUC.death = numeric(length = k)
for(i in 1:k){
  # Test samples
  test.ind = ind[((i-1)*step+1):(i*step)]
  
  # Cox on train samples
  cox.AKI <- coxph(Surv(time.AKI, status.AKI) ~ Male + WBC11 + HGB1012 + HGB10 + CRP100 + DDimer1000, 
                   data=dt[-test.ind,])
  cox.death <- coxph(Surv(time.death, status.death) ~ Age4565 + Age65 + WBC11 + PLT100 + CRP100 + DDimer2000,
                     data=dt[-test.ind,])
  # Test risk scores
  dt.test = dt[test.ind,]
  score.AKI = as.matrix(dt.test[c("Male", "WBC11", "HGB1012", "HGB10", "CRP100", "DDimer1000")]) %*% 
    round(coef(cox.AKI))
  score.death = as.matrix(dt.test[c("Age4565", "Age65", "WBC11", "PLT100", "CRP100", "DDimer1000")]) %*% 
    round(coef(cox.death))
  
  # Test outcomes
  AKI3d = as.numeric(dt.test$status.AKI == 1 & dt.test$time.AKI <= 3)
  Death30d = as.numeric(dt.test$status.death == 1 & dt.test$time.death <= 30)
  
  # Store AUC
  AUC.AKI[i] = auc(roc(AKI3d, as.numeric(score.AKI)))
  AUC.death[i] = auc(roc(Death30d, as.numeric(score.death)))
}

mean(AUC.AKI); min(AUC.AKI); max(AUC.AKI); quantile(AUC.AKI, 0.025); quantile(AUC.AKI, 0.975)
mean(AUC.death); min(AUC.death); max(AUC.death); quantile(AUC.death, 0.025); quantile(AUC.death, 0.975)

######################################
# k iterations 70% training 30% test
######################################

set.seed(83471304)
n = nrow(dt)
k = 1000
n.test = floor(n*0.3) #test percent here
AUC.AKI = AUC.death = numeric(length = k)
for(i in 1:k){
  # Test samples
  test.ind = sample(n, n.test, replace=FALSE)
  
  # Cox on train samples
  cox.AKI <- coxph(Surv(time.AKI, status.AKI) ~ Male + WBC11 + HGB1012 + HGB10 + CRP100 + DDimer1000, 
                   data=dt[-test.ind,])
  cox.death <- coxph(Surv(time.death, status.death) ~ Age4565 + Age65 + WBC11 + PLT100 + CRP100 + DDimer2000,
                     data=dt[-test.ind,])
  # Test risk scores
  dt.test = dt[test.ind,]
  score.AKI = as.matrix(dt.test[c("Male", "WBC11", "HGB1012", "HGB10", "CRP100", "DDimer1000")]) %*% 
    round(coef(cox.AKI))
  score.death = as.matrix(dt.test[c("Age4565", "Age65", "WBC11", "PLT100", "CRP100", "DDimer1000")]) %*% 
    round(coef(cox.death))
  
  # Test outcomes
  AKI3d = as.numeric(dt.test$status.AKI == 1 & dt.test$time.AKI <= 3)
  Death30d = as.numeric(dt.test$status.death == 1 & dt.test$time.death <= 30)
  
  # Store AUC
  AUC.AKI[i] = auc(roc(AKI3d, as.numeric(score.AKI)))
  AUC.death[i] = auc(roc(Death30d, as.numeric(score.death)))
}

mean(AUC.AKI); min(AUC.AKI); max(AUC.AKI); quantile(AUC.AKI, 0.025); quantile(AUC.AKI, 0.975)
mean(AUC.death); min(AUC.death); max(AUC.death); quantile(AUC.death, 0.025); quantile(AUC.death, 0.975)
