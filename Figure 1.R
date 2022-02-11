library(cowplot)

source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/Figure1 ROC.R")
source("C:/Users/Daniel Li/Desktop/COVID Partners/Scripts/Figure1 KM.R")

top = plot_grid(AKI_ROC, Death_ROC, labels = NULL, label_size = 50, nrow=1)
bottom = plot_grid(KM.AKI, KM.death, labels = NULL, label_size = 50, nrow=1)

png(file="C:/Users/Daniel Li/Desktop/COVID Partners/Figure 1.png",width=1600,height=1600)
plot_grid(top, bottom, labels = "AUTO", label_size = 50, nrow=2)
dev.off()
