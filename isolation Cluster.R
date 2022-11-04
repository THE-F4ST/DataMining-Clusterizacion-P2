rm(list = ls())

library("dplyr")
library("magrittr")
library("dbscan")

source("C:/Users/andre/OneDrive/Escritorio/MD Clusters/functions_2.R")

data <- read.csv("C:/Users/andre/OneDrive/Escritorio/MD Clusters/Datasets/isolation (16).csv")

coords <- matrix(c(data$x,data$y), nrow = length(data$x), ncol = 2,
                 byrow = FALSE)

# Notemos que length(which(data$color==0)) != 0
# Por lo tanto:
# Se cambia el color para evitar el color blanco en las graficas y ademas
# para evitar errores de calculo
data$color <- data$color + 1
# Se grafican los clusters reales
plot(coords, col = data$color + 1, xlab = "X", ylab = "Y")

#### POR DBSCAN ####
D_Optimo <- DBSCAN_optimo(coords,
                          TrueClusters = data$color,
                          from = 4, to = 8)
plot(coords, col = D_Optimo$cluster, xlab = "X", ylab = "Y")

T_c <- TablaContingencia(RealClusters = data$color,
                         EstClusters = D_Optimo$cluster)
cat("La medida F para la clusterizacion es: ", medida_f2(T_c),"\n")
cat("La medida de entropia para la clusterizacion es: ", 
    medida_entropia(T_c, length(data$color)), "\n")

# CLUSTERIZACION PERFECTA LETS GOOOOOOOO!!!!!









