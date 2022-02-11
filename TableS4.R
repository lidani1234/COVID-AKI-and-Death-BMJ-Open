source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(cmprsk)

##########################
# Table S3
##########################

cox <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
               Conditions + 
               BMI + Temp + HR+ SBP +
               WBC + HGB + PLT + CRP + Ferritin + DDimer)
sum = format(round(summary(cox)$conf.int, digits = 2), nsmall=2)
cause = paste0(sum[,1], " (", sum[,3], ", ", sum[,4], ")" )

cr <- crr(ftime = time.AKI.cr, fstatus = status.AKI.cr, cov1 = model.matrix(cox))
sum = summary(cr, exp=T)

var = colnames(model.matrix(cox))
comp = paste0(format(round(sum$conf.int[,1], digits = 2), nsmall=2), 
              " (",
              format(round(sum$conf.int[,3], digits = 2), nsmall=2), 
              ", ",
              format(round(sum$conf.int[,4], digits = 2), nsmall=2), 
              ")")

write.csv(cbind(var, cause, comp), 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/TableS4.csv",
          row.names=F)
