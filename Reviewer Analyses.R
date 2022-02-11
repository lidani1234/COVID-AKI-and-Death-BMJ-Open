source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(pROC)

###############################
# Round 1 review
###############################

# AKI stages sensitivity analysis
sum(status.AKI)
summary(stage.AKI)
round(summary(stage.AKI) / sum(status.AKI), digits = 3)

sum(stage23.AKI)
sum(stage23.AKI == 1 & status.death == 1)
sum(stage23.AKI == 1 & status.death == 1) / sum(stage23.AKI)

# Risk scores
Age4565 = as.numeric(Age == "45-65")
Age65 = as.numeric(Age == ">65")
WBC11 = as.numeric(data$WBC > 11)
HGB10 = as.numeric(data$HGB < 10)
CRP100 = as.numeric(data$CRP > 100)
DDimer1000 = as.numeric(data$DDimer >= 1000)
DDimer2000 = as.numeric(data$DDimer >= 2000)

score.AKI = as.numeric(HGB=="<10")*2 + as.numeric(HGB=="10-12")*1 +
  + DDimer1000*1 + CRP100*1 + WBC11 + as.numeric(Sex=="Male")*1 
score.death = Age65*3 + Age4565*2 + CRP100*1 + as.numeric(PLT=="<100")*1 + 
  WBC11*1 + DDimer1000*1

# Binary outcomes
AKI = as.numeric(stage23.AKI == 1)
Death = as.numeric(status.death == 1)

# Categorical AUC's
auc(roc(AKI, score.AKI)); ci(roc(AKI, score.AKI))
ind = which(stage23.AKI == 1)
auc(roc(Death[ind], score.death[ind])); ci(roc(Death[ind], score.death[ind]))

###############################
# Round 2 review, see Paper Stats.R script and Table1.R script
###############################

