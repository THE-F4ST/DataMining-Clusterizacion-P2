rm(list = ls())

library("dplyr")
library("magrittr")
library("dbscan")

source("C:/Users/andre/OneDrive/Escritorio/MD Clusters/functions_2.R")

data <- read.csv("C:/Users/andre/OneDrive/Escritorio/MD Clusters/Datasets/boxes (8).csv")

# Se obtienen coordenadas de los puntos del dataset
coords <- matrix(c(data$x,data$y), nrow = length(data$x), ncol = 2,
                 byrow = FALSE)

# Notemos que 
# length(which(data$color==0)) != 0
# Por lo tanto:
# Se cambia el color para evitar el color blanco en las graficas y ademas
# para evitar errores de calculo
data$color <- data$color + 1
# Se grafican los clusters reales
plot(coords, col = data$color + 1, xlab = "X", ylab = "Y")

# Se busca una clusterizacion optima
Optimo <- DBSCAN_optimo(coords, data$color, from = 8, to = 12, by = 1)

# Se grafican los clusters estimados
plot(coords, col=Optimo$cluster, xlab = "X", ylab = "Y")

T_c <- TablaContingencia(RealClusters = data$color,
                         EstClusters = Optimo$cluster)

# Conclusion
cat("La medida F de la clusterizacion es: ", medida_f2(T_c), "\n")
cat("La medida de entropia de la clusterizacion es: ",
    medida_entropia(T_c, length(data$x)), "\n")

#CantidadClusters(Optimo$cluster)
# Clusterización casi perfecta

