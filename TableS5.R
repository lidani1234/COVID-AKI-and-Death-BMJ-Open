source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(mice)

##########################
# Table 2
##########################

# data frame setup
dt = data.frame(time.AKI=time.AKI, status.AKI=status.AKI,
                time.death=time.death, status.death=status.death,
                Age=Age, Sex=Sex, Race=Race,
                Conditions=Conditions,
                BMI=BMI, Temp=Temp, HR=HR, SBP=SBP, RR=RR,
                WBC=WBC, HGB=HGB, PLT=PLT, 
                CRP=CRP, Ferritin=Ferritin, DDImer=DDimer, IL6=IL6)

# variable reformatting
dt$Sex = as.factor(dt$Sex)
dt$RR[dt$RR == "NA"] = NA
dt$RR = as.factor(as.character(dt$RR))
dt$IL6[dt$IL6 == "NA"] = NA
dt$IL6 = as.factor(as.character(dt$IL6))
summary(dt)

# imputation
imp.dt = mice(dt, m=10, seed=800)

# Table S3
# AKI survival regression on imputated data
fit <- with(data = imp.dt, 
            exp=coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
                                       Conditions + 
                                       BMI + Temp + HR+ SBP + RR +
                                       WBC + HGB + PLT + CRP + Ferritin + DDimer + IL6))
combine <- pool(fit)
sum = summary(combine)

t = qt(0.975, sum$df)
res = data.frame(term=sum$term,
                 estimate=round(exp(sum$estimate), digits=2),
                 lowerCI=round(exp(sum$estimate - t*sum$std.error), digits=2),
                 upperCI=round(exp(sum$estimate + t*sum$std.error), digits=2),
                 pvalue=round(sum$p.value, digits=3))

var = as.character(res$term)
missA = paste0(format(round(res$estimate, digits = 2), nsmall=2), 
              " (",
              format(round(res$lowerCI, digits = 2), nsmall=2), 
              ", ",
              format(round(res$upperCI, digits = 2), nsmall=2), 
              ")")


# Table S4
# Death survival regression on imputated data
fit <- with(data = imp.dt, 
            exp=coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
                        Conditions + 
                        BMI + Temp + HR+ SBP + RR +
                        WBC + HGB + PLT + CRP + Ferritin + DDimer + IL6))
combine <- pool(fit)
sum = summary(combine)

t = qt(0.975, sum$df)
res = data.frame(term=sum$term,
                 estimate=round(exp(sum$estimate), digits=2),
                 lowerCI=round(exp(sum$estimate - t*sum$std.error), digits=2),
                 upperCI=round(exp(sum$estimate + t*sum$std.error), digits=2),
                 pvalue=round(sum$p.value, digits=3))

var = res$term
missD = paste0(format(round(res$estimate, digits = 2), nsmall=2), 
               " (",
               format(round(res$lowerCI, digits = 2), nsmall=2), 
               ", ",
               format(round(res$upperCI, digits = 2), nsmall=2), 
               ")")

write.csv(cbind(var, missA, missD), 
          file="C:/Users/Daniel Li/Desktop/COVID Partners/TableS5.csv",
          row.names=F)
