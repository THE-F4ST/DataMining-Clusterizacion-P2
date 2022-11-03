rm(list = ls())

library("dplyr")
library("magrittr")
library("dbscan")

source("C:/Users/andre/OneDrive/Escritorio/MD Clusters/functions.R")

data <- read.csv("C:/Users/andre/OneDrive/Escritorio/MD Clusters/Datasets/isolation (16).csv")

coords <- matrix(c(data$x,data$y), nrow = length(data$x), ncol = 2,
                 byrow = FALSE)

# Se cambia el color para evitar el color blanco en las graficas
data$color <- data$color + 1
# Se grafican los clusters reales
plot(coords, col = data$color + 1, xlab = "X", ylab = "Y")

#### Densidad ####
# Se puede intuir que un clster basado en densidad sería malo
# a pesar de esto se intenta

D_Optimo <- dbscan(coords, eps = 4, minPts = 6)

plot(coords, col = D_Optimo$cluster, xlab = "X", ylab = "Y")

T_c <- TablaContingencia(RealClusters = data$color,
                         EstClusters = D_Optimo$cluster)

#### POR KMEANS ####
# Se intuye que no se obtendrá una buena medida F, a pesar de esto
# se intenta y se observa que enefecto, no funcionó
Optimo <- kmeans(coords, centers = 3, iter.max = 1000)
# Se grafican los clusters estimados
plot(coords, col=Optimo$cluster, xlab = "X", ylab = "Y")
T_c <- TablaContingencia(RealClusters = data$color,
                         EstClusters = Optimo$cluster)
medida_f2(T_c)



#### Por algún otro ####
