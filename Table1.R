source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

# Number of AKI and non-AKI patients
summary(as.factor(status.AKI))
round(summary(as.factor(status.AKI)) / length(status.AKI), digits = 3)

# Number of death and non death patients
summary(as.factor(status.death))
round(summary(as.factor(status.death)) / length(status.death), digits = 3)

# Stats function
stats <- function(x){
  list(AKI = rbind(summary(x[status.AKI==1]), round(summary(x[status.AKI==1])/length(x[status.AKI==1])*100, digits = 1)),
       death = rbind(summary(x[status.death==1]), round(summary(x[status.death==1])/length(x[status.death==1])*100, digits = 1)),
       all = rbind(summary(x), round(summary(x)/length(x)*100, digits = 1)))
}

# Automated table 1 csv file
dt = data.frame(Age=Age, Sex=as.factor(Sex), Race=Race, 
                Diabetes=Diabetes, CVD=CVD, HTN=HTN, HF=HF, Conditions=Conditions,
                BMI=BMI, Temp=Temp, HR=HR, SBP=SBP, RR=RR,
                WBC=WBC, Lymphopenia=Lymphopenia, HGB=HGB, PLT=PLT, 
                CRP=CRP, Ferritin=Ferritin, DDimer=DDimer, IL6=IL6,
                MV=MV)

p = ncol(dt)
table = matrix(NA, nrow=1, ncol=5)
colnames(table) = c("Variable", "Level", "AKI", "Death", "All")
for(i in 1:20){
  counts = stats(dt[,i])
  add = cbind(colnames(dt)[i],
              colnames(counts$AKI),
              t(rbind(paste0(round(counts$AKI[1,], digits=0), " (", format(counts$AKI[2,], nsmall=1), ")"),
                      paste0(round(counts$death[1,], digits=0), " (", format(counts$death[2,], nsmall=1), ")"),
                      paste0(round(counts$all[1,], digits=0), " (", format(counts$all[2,], nsmall=1), ")"))))
  table = rbind(table, add)
}

write.csv(table[-1,], 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/Table1.csv",
          row.names=F)

# Individual table 1 entries
stats(Age)
stats(as.factor(Sex))
stats(Race)
stats(Diabetes)
stats(CVD)
stats(HTN)
stats(HF)
stats(Conditions)
stats(BMI)
stats(Temp)
stats(HR)
stats(SBP)
stats(RR)
stats(WBC)
stats(Lymphopenia)
stats(HGB)
stats(PLT)
stats(CRP)
stats(Ferritin)
stats(DDimer)
stats(IL6)
stats(MV)
