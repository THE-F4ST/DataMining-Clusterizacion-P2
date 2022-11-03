rm(list = ls())

library("dplyr")
library("magrittr")
library("dbscan")

source("C:/Users/andre/OneDrive/Escritorio/MD Clusters/functions.R")

data <- read.csv("C:/Users/andre/OneDrive/Escritorio/MD Clusters/Datasets/spiral2 (24).csv")

coords <- matrix(c(data$x,data$y), nrow = length(data$x), ncol = 2,
                 byrow = FALSE)

# Se cambia el color para evitar el color blanco en las graficas
data$color <- data$color + 1
# Se grafican los clusters reales
plot(coords, col = data$color + 1, xlab = "X", ylab = "Y")



# Se busca una clusterizacion optima
Optimo <- DBSCAN_optimo(coords, data$color, from = 5, to = 8, by = 1)

# Se suma 1 para evitar cluster 0 para evitar errores al momento
# de calcular la tabla de contingencia, ademas así evitamos el color
# blanco al momento de graficar los clusters
Optimo$cluster <- Optimo$cluster + 1

# Se grafican los clusters estimados
plot(coords, col=Optimo$cluster, xlab = "X", ylab = "Y")

T_c = TablaContingencia(RealClusters = data$color, 
                        EstClusters = Optimo$cluster)

## Notemos que sin embargo se obtiene una buena medida de entropia
cat("El valor de la medida F es: ", medida_f2(T_c), "\n")
cat("El valor de la medida de entropia es: ", 
    medida_entropia(T_c, length(data$x)), "\n")
# Notemos que la medida F es mala pero la entropia es muy buena

#### --------- K-MEANS --------- ####
K_Optimo <- kmeans(coords, 2, iter.max = 500)
plot(coords, col=K_Optimo$cluster, xlab = "X", ylab = "Y")
T_c2 <- TablaContingencia(RealClusters = data$color,
                          EstClusters = K_Optimo$cluster)

cat("El valor de la medida F es: ", medida_f2(T_c2), "\n")
cat("El valor de la medida de entropia es: ", 
    medida_entropia(T_c2, length(data$x)), "\n")
# Notemos que la medida F es muy buena y la de entropia es buena
# pero empeoró en comparasión con dbscan

# En lo  personal prefiero tomar en este caso la medida de entropia
# para la toma de decisión de la clusterización
# Nota: Podemos concluir que la medida F penaliza mucho clusters
#       que no sean tan buenos como lo fue C3
