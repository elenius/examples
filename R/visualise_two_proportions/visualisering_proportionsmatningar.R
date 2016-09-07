library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

pm.colors <- c("#E34912", #röd/orange
               "#EF8200", #orange
               "#FFB70F", #gul
               "#F6CF47", #blekare gul
               "#AC1A2F", #Röd/rosa
               "#D53044", #blekare röd/rosa
               "#870150", #lila/rosa
               "#C42695", #rosa
               "#125687", #mörkblå
               "#5EB0E6", #ljusblå
               "#545F1D", #mörkgrön
               "#AFA500", #limegrön
               "#766A63", #mörkgrå
               "#B9B1A9")


# parametervärden
p1 <- 0.75
n1 <- 120

p2 <- 0.75
n2 <- 120

nview <- 1 # 12

set.seed(160906)

source("R/visualisering_proportionsmatningar_source.R", encoding = "utf-8")

grid.arrange(plot_1, plot_2,  ncol = 1)





