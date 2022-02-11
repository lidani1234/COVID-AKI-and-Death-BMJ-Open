source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

######################################
# Simplified Risk scores
######################################

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
cox.death.simp <- coxph(Surv(time.death, status.death) ~ Age + WBC11 + PLT + CRP100 + DDimer2000)

# Simplified scores
AKI.coef = coef(cox.AKI.simp)
ind = order(-AKI.coef)
AKI.coef[ind]
(AKI.coef / AKI.coef[5])[ind]
score.AKI = as.numeric(HGB=="<10")*2 + as.numeric(HGB=="10-12")*1 +
  + DDimer1000*1 + CRP100*1 + WBC11 + as.numeric(Sex=="Male")*1 
   

death.coef = coef(cox.death.simp)
ind = order(-death.coef)
death.coef[ind]
(death.coef / death.coef[5])[ind]
score.death = Age65*3 + Age4565*2 + CRP100*1 + as.numeric(PLT=="<100")*1 + 
  WBC11*1 + DDimer1000*1

######################################
# Fully adjusted risk score
######################################

# AKI Cox Regression
cox <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
               Conditions +
               BMI + Temp + HR+ SBP +
               WBC + HGB + PLT + CRP + Ferritin + DDimer)
AKI.HR = round(cox$coefficients, digits = 2)
var = names(AKI.HR)

# Death Cox regression
cox <- coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
               Conditions + 
               BMI + Temp + HR+ SBP +
               WBC + HGB + PLT + CRP + Ferritin + DDimer)
Death.HR = round(cox$coefficients, digits = 2)

write.csv(cbind(var, AKI.HR, Death.HR), 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/TableS2.csv",
          row.names=F)

######################################
# Sensitivity and Specificity Table
######################################

# Risk score and AKI
D = as.numeric(status.AKI == 1 & time.AKI <= 3)
M = score.AKI
c = seq(from=0, to=7, by=1)
n = length(D)
m = length(c)
ind.D0 = which(D == 0)
sens = spec = ppv = npv = numeric(length = m)
for(i in 1:m){
  ind = which(M >= c[i])
  n.pos = length(ind)
  sens[i] = sum(D[ind]) / sum(D)
  spec[i] = 1 - sum(ind %in% ind.D0) / (n - sum(D))
  ppv[i] = sum(D[ind]) / n.pos
  npv[i] = (n - sum(D) - sum(ind %in% ind.D0)) / (n - n.pos)
}
results1 = data.frame(Outcome=rep("AKI", m),
                      Lab=rep("RiskScore", m),
                      Cutoff=paste(">=", c),
                      Sensitivity=round(sens, digits = 3),
                      Specificity=round(spec, digits = 3),
                      LR.plus = round(sens/(1-spec), digits = 2),
                      LR.minus = round((1-sens)/spec, digits = 2),
                      PPV=round(ppv, digits=3),
                      NPV=round(npv, digits=3))
results1$Prob.plus = 0.19*log(results1$LR.plus)
results1$Prob.minus = 0.19*log(results1$LR.minus)
results1
results1

# Risk score and death
D = as.numeric(status.death == 1 & time.death <= 30)
M = score.death
c = seq(from=0, to=8, by=1)
n = length(D)
m = length(c)
ind.D0 = which(D == 0)
sens = spec = ppv = npv = numeric(length = m)
for(i in 1:m){
  ind = which(M >= c[i])
  n.pos = length(ind)
  sens[i] = sum(D[ind]) / sum(D)
  spec[i] = 1 - sum(ind %in% ind.D0) / (n - sum(D))
  ppv[i] = sum(D[ind]) / n.pos
  npv[i] = (n - sum(D) - sum(ind %in% ind.D0)) / (n - n.pos)
}
results2 = data.frame(Outcome=rep("Death", m),
                      Lab=rep("RiskScore", m),
                      Cutoff=paste(">=", c),
                      Sensitivity=round(sens, digits = 3),
                      Specificity=round(spec, digits = 3),
                      LR.plus = round(sens/(1-spec), digits = 2),
                      LR.minus = round((1-sens)/spec, digits = 2),
                      PPV=round(ppv, digits=3),
                      NPV=round(npv, digits=3))
results2$Prob.plus = 0.19*log(results2$LR.plus)
results2$Prob.minus = 0.19*log(results2$LR.minus)
results2

full = rbind(results1, results2)

write.csv(full, 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/TableS3.csv",
          row.names=FALSE)
