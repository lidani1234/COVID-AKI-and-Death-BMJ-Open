source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(survminer)
require(mstate)

######################################
# Risk scores
######################################

# Variables
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

######################################
# AKI KM Curves
######################################

n = nrow(data)
summary(score.AKI)
round(summary(as.factor(score.AKI)) / n * 100)
score = rep("4-6", nrow(data))
score[score.AKI <= 3] = "2-3"
score[score.AKI <= 1] = "0-1"
dt = data.frame(time.AKI = time.AKI, status.AKI = status.AKI, score=score)
km_fit <- survfit(Surv(time.AKI, status.AKI) ~ score, data = dt)

km_fit
round(c(83/1604, 275/1494, 338/618)*100, digits = 1)
summary(km_fit, times=c(3,30))

KM.AKI = 
  ggsurvplot(km_fit, 
             fun="event", 
             conf.int = TRUE, 
             ylim=c(0, 0.65),
             title="AKI Event Curves by AKI Risk Score",
             font.title = c(35),
             font.x = c(25),
             font.y = c(25),
             font.tickslab = c(25),
             font.legend = c(25),
             legend="bottom",
             legend.title="Risk Score",
             legend.labs=c("Low", "Medium", "High"),
             risk.table="absolute",
             risk.table.title = "No. at risk",
             tables.height = 0.15,
             fontsize = c(8),
             tables.theme = theme_cleantable()) +
  xlab("Days") +
  ylab("AKI proportion")
KM.AKI$table = KM.AKI$table + 
  theme(plot.title = element_text(size = 25),
        axis.text.y = element_text(size = 25)) 
KM.AKI
survdiff(Surv(time.AKI, status.AKI) ~ score)

######################################
# Death KM Curves
######################################

summary(score.death)
round(summary(as.factor(score.death)) / n * 100)
score = rep("5-7", nrow(data))
score[score.death <= 4] = "4"
score[score.death <= 3] = "0-3"
dt = data.frame(time.death = time.death, status.death = status.death, score=score)
km_fit <- survfit(Surv(time.death, status.death) ~ score, data = dt)

km_fit
round(c(39/2291, 88/845, 220/580)*100, digits = 1)
summary(km_fit, times=c(30,60))

KM.death = 
  ggsurvplot(km_fit, 
             fun="event", 
             conf.int = TRUE, 
             xlim=c(0, 60),
             title="Death Event Curves by Death Risk Score",
             font.title = c(35),
             font.x = c(25),
             font.y = c(25),
             font.tickslab = c(25),
             font.legend = c(25),
             legend="bottom",
             legend.title="Risk Score",
             legend.labs=c("Low", "Medium", "High"),
             risk.table="absolute",
             risk.table.title = "No. at risk",
             tables.height = 0.15,
             fontsize = c(8),
             tables.theme = theme_cleantable()) +
  xlab("Days") +
  ylab("Death proportion")
KM.death$table = KM.death$table + 
  theme(plot.title = element_text(size = 25),
        axis.text.y = element_text(size = 25)) 
KM.death
survdiff(Surv(time.death, status.death) ~ score)

png(file="C:/Users/Daniel Li/Desktop/COVID Partners/Figure 1 - KM Plots.png",width=1600,height=800)
arrange_ggsurvplots(list(KM.AKI, KM.death), nrow=1, ncol=2)
dev.off()
