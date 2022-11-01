rm(list = ls())

library("dplyr")
library("magrittr")
library("dbscan")

source("C:/Users/andre/OneDrive/Escritorio/MD Clusters/functions.R")

data <- read.csv("C:/Users/andre/OneDrive/Escritorio/MD Clusters/spiral2 (24).csv")

plot(data$x, data$y, col=data$color+1)

####  ####
C1 <- dbscan(data, eps = 8, minPts = 6)
plot(data$x, data$y, col = C1$cluster + 1, xlab = "X", ylab = "Y")
T <- TablaContingencia(data$color, C1$cluster)
print(C1$cluster)
