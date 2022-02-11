source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

# AKI Cox Regression
cox <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
               Conditions +
               BMI + Temp + HR+ SBP +
               WBC + HGB + PLT + CRP + Ferritin + DDimer)
sum = format(round(summary(cox)$conf.int, digits = 2), nsmall=2)
AKI.HR = paste0(sum[,1], " (", sum[,3], ", ", sum[,4], ")" )
var = rownames(sum)

# Death Cox regression
cox <- coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
               Conditions + 
               BMI + Temp + HR+ SBP +
               WBC + HGB + PLT + CRP + Ferritin + DDimer)
sum = summary(cox)$conf.int
Death.HR = paste0(format(round(sum[,1], digits = 2), nsmall=2), 
                  " (", 
                  format(round(sum[,3], digits = 2), nsmall=2), 
                  ", ", 
                  format(round(sum[,4], digits = 2), nsmall=2),
                  ")" )

write.csv(cbind(var, AKI.HR, Death.HR), 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/Table2.csv",
          row.names=F)
