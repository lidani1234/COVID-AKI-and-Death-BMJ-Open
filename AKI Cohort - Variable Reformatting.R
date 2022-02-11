data = read.csv("Z:/2020P001661/AKI cohort/AKI cohort for Dr. Lin's lab 01152021.csv",
                fileEncoding="UTF-8-BOM")
data = subset(data, MH_CKD == 0)


#################################
# Univariable AKI survival analysis
#################################

library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

#AKI survival object
ind = data$DeathDTS_y == ""
date.end = as.character(data$DeathDTS_y)
date.end[ind] = "5/28/2020"
as.Date(date.end, "%m/%d/%Y")

date.beg = unlist(strsplit(as.character(data$COVID_ORDER_DTS), " "))[1:nrow(data)*2-1]

time.death = as.numeric(as.Date(date.end, "%m/%d/%Y") - as.Date(date.beg, "%m/%d/%Y"))
time.death[which(time.death > 30)] = 30

time.AKI = data$TimetoAKI_h/24
time.AKI[is.na(time.AKI)] = time.death[is.na(time.AKI)]
status.AKI = data$AKI

#Death survival object
ind = data$DeathDTS_y == ""
date.end = as.character(data$DeathDTS_y)
date.end[ind] = "5/28/2020"
as.Date(date.end, "%m/%d/%Y")

date.beg = unlist(strsplit(as.character(data$COVID_ORDER_DTS), " "))[1:nrow(data)*2-1]

time.death = as.numeric(as.Date(date.end, "%m/%d/%Y") - as.Date(date.beg, "%m/%d/%Y"))
ind = time.death < time.AKI #from date rounding errors
time.death[ind] = time.AKI[ind]

status.death = data$Death_y
status.death[is.na(status.death)] = 0

# AKI Competing Risks Survival object
ind = ((status.AKI == 0) + (status.death==1) + (time.death <= 30)) == 3
status.AKI.cr = status.AKI
status.AKI.cr[ind] = 2
time.AKI.cr = time.AKI

# AKI stage variable
stage.AKI = status.AKI
stage.AKI[which(data$CRE_VAL >= (2 * data$CRE_REF_VAL))] = 2
stage.AKI[which(data$CRE_VAL >= (3 * data$CRE_REF_VAL))] = 3
stage.AKI[which(data$CRE_VAL >= 4 & status.AKI == 1)] = 3
stage.AKI[which(data$CRRT == 1 & status.AKI == 1)] = 3
stage.AKI = as.factor(stage.AKI)

stage23.AKI = (stage.AKI == "2" | stage.AKI == "3") 

# Age
Age <- rep("45-65", nrow(data))
Age[data$Age <= 45] <- "<45"
Age[data$Age >= 65] <- ">65"
Age = as.factor(Age)
Age = relevel(Age, ref="45-65")
Age = relevel(Age, ref="<45")

# Sex
Sex <- ifelse(data$Sex == 0, "Female", "Male")

# Race
Race <- rep("White", nrow(data))
Race[data$Race_simp == 2] <- "Black"
Race[data$Race_simp == 3] <- "Hispanic"
Race[data$Race_simp == 4] <- "Asian"
Race[data$Race_simp == 5] <- "Other"
Race = as.factor(Race)
Race = relevel(Race, ref="Hispanic")
Race = relevel(Race, ref="Black")
Race = relevel(Race, ref="White")

# Diabetes
Diabetes <- as.factor(ifelse(data$MH_DM == 0, "No", "Yes"))

# CKD
# CKD <- as.factor(ifelse(data$MH_CKD == 0, "No", "Yes"))

# CVD
CVD <- as.factor(ifelse(data$MH_CVD == 0, "No", "Yes"))

# HTN
HTN <- as.factor(ifelse(data$MH_Hypertension == 0, "No", "Yes"))

# HF
HF <- as.factor(ifelse(data$MH_HF == 0, "No", "Yes"))

# Number of medical conditions
data$Conditions = data$MH_DM + data$MH_CKD + data$MH_CVD + data$MH_Hypertension + data$MH_HF
Conditions <- rep("0", nrow(data))
Conditions[data$Conditions >=1 ] <- "1"
Conditions[data$Conditions >=2 ] <- "2+"
Conditions[data$Conditions >=3 ] <- "3+"
Conditions = as.factor(Conditions)

# BMI
BMI <- rep("25-30", nrow(data))
BMI[data$BMI <= 25] <- "<25"
BMI[data$BMI >= 30] <- ">30"
BMI = as.factor(BMI)
BMI = relevel(BMI, ref="25-30")
BMI = relevel(BMI, ref="<25")

# Temp
Temp <- rep("97-100.4", nrow(data))
Temp[data$Vital_Temp <= 97] <- "<97"
Temp[data$Vital_Temp >= 100.4] <- ">100.4"
Temp = as.factor(Temp)
Temp = relevel(Temp, ref="97-100.4")

# HR
HR <- rep("60-110", nrow(data))
HR[data$Vital_HR <= 60] <- "<60"
HR[data$Vital_HR >= 110] <- ">110"
HR = as.factor(HR)
HR = relevel(HR, ref="60-110")

# SBP
SBP <- rep("90-180", nrow(data))
SBP[data$Vital_SBP <= 90] <- "<90"
SBP[data$Vital_SBP >= 180] <- ">180"
SBP = as.factor(SBP)
SBP = relevel(SBP, ref="90-180")

# RR
RR <- rep("NA", nrow(data))
RR[data$Vital_RespRate_new <= 20] <- "<20"
RR[data$Vital_RespRate_new > 20] <- ">20"
RR = as.factor(RR)
RR = relevel(RR, ref=">20")
RR = relevel(RR, ref="<20")

# WBC
WBC <- rep("3.5-11", nrow(data))
WBC[data$WBC <= 3.5] <- "0-3.5"
WBC[data$WBC >= 11] <- "11+"
WBC = as.factor(WBC)
WBC = relevel(WBC, ref="0-3.5")
WBC = relevel(WBC, ref="3.5-11")

# Hemoglobin
HGB <- rep("10-12", nrow(data))
HGB[data$HGB <= 10] <- "<10"
HGB[data$HGB >= 12] <- ">12"
HGB = as.factor(HGB)
HGB = relevel(HGB, ref="10-12")
HGB = relevel(HGB, ref=">12")

# Platelet
#PLT <- rep("100-400", nrow(data))
PLT <- rep(">100", nrow(data))
PLT[data$PLT <= 100] <- "<100"
#PLT[data$PLT >= 400] <- ">400"
PLT = as.factor(PLT)
PLT = relevel(PLT, ref=">100")
#PLT = relevel(PLT, ref="100-400")

# CRP
CRP <- rep("50-100", nrow(data))
CRP[data$CRP <= 50] <- "<50"
CRP[data$CRP >= 100] <- ">100"
CRP = as.factor(CRP)
CRP = relevel(CRP, ref="50-100")
CRP = relevel(CRP, ref="<50")

# Ferritin
Ferritin <- rep("250-1000", nrow(data))
Ferritin[data$Ferritin <= 250] <- "<250"
Ferritin[data$Ferritin >= 1000] <- ">1000"
Ferritin = as.factor(Ferritin)
Ferritin = relevel(Ferritin, ref="250-1000")
Ferritin = relevel(Ferritin, ref="<250")

# DDimer
DDimer <- rep("1000-2000", nrow(data))
DDimer[data$DDimer <= 1000] <- "<1000"
DDimer[data$DDimer >= 2000] <- ">2000"
DDimer = as.factor(DDimer)
DDimer = relevel(DDimer, ref="1000-2000")
DDimer = relevel(DDimer, ref="<1000")

# IL6
IL6 <- rep("NA", nrow(data))
IL6[data$IL6 <= 40] <- "<40"
IL6[data$IL6 > 40] <- "40-80"
IL6[data$IL6 >= 80] <- ">80"
IL6 = as.factor(IL6)
IL6 = relevel(IL6, ref=">80")
IL6 = relevel(IL6, ref="40-80")
IL6 = relevel(IL6, ref="<40")

# Binary events
AKI3d = as.numeric(status.AKI == 1 & time.AKI <= 3)
Death30d = as.numeric(status.death == 1 & time.death <= 30)

# Reviewer suggested variables
MV = as.factor(data$MV)
Lymphopenia = as.factor(as.numeric(data$Lymph. <= 0.8))