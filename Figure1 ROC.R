source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/AKI Cohort - Variable Reformatting.R")

require(plotROC)
require(pROC)
require(cowplot)
require(plyr)

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
# Cox model predictions
######################################

# Model predictions
cox.AKI <- coxph(Surv(time.AKI, status.AKI) ~ Age + Sex + Race + 
                   Conditions + 
                   BMI + Temp + HR+ SBP +
                   WBC + HGB + PLT + CRP + Ferritin + DDimer)
pred.AKI.all = cox.AKI$linear.predictors

cox.death <- coxph(Surv(time.death, status.death) ~ Age + Sex + Race + 
                     Conditions + 
                     BMI + Temp + HR+ SBP +
                     WBC + HGB + PLT + CRP + Ferritin + DDimer)
pred.death.all = cox.death$linear.predictors

#cox.AKI <- coxph(Surv(time.AKI, status.AKI) ~ Sex + HGB + CRP100 + DDimer1000)
#pred.AKI = cox.AKI$linear.predictors
pred.AKI = score.AKI

#cox.death <- coxph(Surv(time.death, status.death) ~ Age65 + WBC11 + PLT + CRP100 + DDimer2000)
#pred.death = cox.death$linear.predictors
pred.death = score.death

######################################
# ROC Curves
######################################

data$AKI3d = as.numeric(status.AKI == 1 & time.AKI <= 3)

# Default greater than cutoff, so reformat for multiple plots
data$"HGB + DDimer + CRP + WBC + Male" = pred.AKI
data$All = pred.AKI.all

# AKI ROC plot
longtest <- melt_roc(data, "AKI3d", c("All", "HGB + DDimer + CRP + WBC + Male" ))
names(longtest)[3] <- "Variable"

AKI_ROC <- ggplot(longtest, aes(d = D, m = M, color = Variable)) + 
  geom_roc(n.cuts = 0) + 
  style_roc() +
  theme_bw() + 
  theme(plot.title = element_text(size = 35),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 25),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="bottom",
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(labels = c("All", "HGB + DDimer + CRP + WBC + Male"), 
                     values = c("cornflowerblue", "green2")) +
  annotate(geom="text", x=0.70, y=0.15, cex=10,
           label="AUC: 0.82 (95% CI: 0.79, 0.85)", colour="cornflowerblue") +
  annotate(geom="text", x=0.70, y=0.10, cex=10,
           label="AUC: 0.79 (95% CI: 0.76, 0.81)", colour="green2") +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("AKI in 3 Days ROC Curves") +
  labs(fill = "Variable")
AKI_ROC

# Death ROC Curves
data$Death = as.numeric(status.death == 1 & time.death <= 30)
data$All = pred.death.all
data$"Age + CRP + PLT + WBC + DDimer" = pred.death
longtest <- melt_roc(data, "Death", c("All", "Age + CRP + PLT + WBC + DDimer"))
names(longtest)[3] <- "Variable"
longtest$Variable = factor(longtest$Variable, levels = c("All", "Age + CRP + PLT + WBC + DDimer"))

Death_ROC <- ggplot(longtest, aes(d = D, m = M, color = Variable)) + 
  geom_roc(n.cuts = 0) + 
  style_roc() +
  theme_bw() + 
  theme(plot.title = element_text(size = 35),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 25),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="bottom",
        legend.key.width = unit(2, "cm")) +
  scale_color_manual(labels = c("All", "Age + CRP + PLT + WBC + DDimer"), 
                     values = c("cornflowerblue", "green2")) +
  annotate(geom="text", x=0.70, y=0.15, cex=10,
           label="AUC: 0.89 (95% CI: 0.88, 0.91)", colour="cornflowerblue") +
  annotate(geom="text", x=0.70, y=0.10, cex=10,
           label="AUC: 0.86 (95% CI: 0.84, 0.88)", colour="green2") +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Death in 30 Days ROC Curves") +
  labs(fill = "Variable")
Death_ROC

png(file="C:/Users/Daniel Li/Desktop/COVID Partners/Figure 1 - ROC Plots.png",width=1600,height=800)
plot_grid(AKI_ROC, Death_ROC, nrow=1, labels=NULL, label_size = 40)
dev.off()
