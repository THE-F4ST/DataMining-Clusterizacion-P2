
#### Función para tabla de contingencia ####
# Recibe vectores de clusters reales y clusters estimados 
# Regresa data frame de la tabla de contingencia
TablaContingencia <- function(RealClusters, EstClusters, df_on = FALSE){
  
  # Se obtienen cantidad de clusters reales y estimados
  ECn <- CantidadClusters(EstClusters)
  RCn <- CantidadClusters(RealClusters)
  # Obtiene la longitud de los datos
  w <- length(RealClusters)
  
  # Se genera matriz que será la tabla de contingencia con datos nulos
  Tabla <- matrix(data = NA, 
                  nrow = ECn,
                  ncol = RCn)
  
  # Ciclos para llenado de la tabla
  for(i in 1:ECn){
    for(j in 1:RCn){
      n <- 0
      for(k in 1:w){
        if(EstClusters[k] == i){
          if(RealClusters[k] == j){
            n <- n + 1
          }
        }
      }
      Tabla[i,j] <- n
    }
  }
  
  # Se convierte la tabla a data frame para una mejor visualizacion
  if(df_on == TRUE){
    Tabla <- data.frame(Tabla) 
  }

  return(Tabla)  
}

#### Función que cuenta la cantidad de clusters distintos ####
# Recibe un vector de clusters NO vacio 
# Regresa la cantidad de clusters distintos
CantidadClusters <- function(C){
  n <- C[1]
  for(i in 2:length(C)){
    if(!isInVec(C[i], n)){
      n <- c(n, C[i])
    }
  }
  return(length(n))
}

# Función que verifica si un dato x está en un vector
# Recibe un dato y un vector
# Regresa Tvalor booleano 
isInVec <- function(x, vec){
  for(i in 1:length(vec)){
    if(x == vec[i]){
      return(TRUE)
    }
  }
  return(FALSE)
}





#### Variacion de parametros DBSCAN ####
DBSCAN_optimo <- function(Datos, TrueClusters, by = 1, from = 1, to = 10){
  # Quiero hacer una función que minimice el error de una tabla 
  # de contingencia. Para ello necesito saber que medida usar,
  # Comparar tablas para ciertos valores de eps y minPts y 
  # regresarlo.
  
  C_M <- NA     # Vector que almacenará la clusterización que
                # maximice la medida 
  F_M <- 0      # Valor de medida F para C_M (criterio de selección)
  eps <- NA     # Almacena configuración de eps
  minPts <- NA  # Almacena configuracion de minPts
  
  C_temp <- NA    # Vector temporal de clusterización
  F_temp <- NA    # medida f temporal para criterio de seleccion
  
  cat(" eps | minPts\t F calculado\t F maximo\n")
  for(i in seq(from = from, to = to, by = by)){
    for(j in seq(from = from, to = to, by = by)){
      C_temp <- dbscan(Datos, eps = i, minPts = j)
      Tabla_temp <- TablaContingencia(TrueClusters, C_temp$cluster)
      F_temp <- medida_f2(Tabla_temp)
      cat("(",i,",",j,")\t",F_temp,"\t", F_M, "\n")
      if(F_temp > F_M){
        F_M <- F_temp
        C_M <- C_temp
      }
    }
  }
  
  # Resumen
  cat("\n----------------------- Resumen del algoritmo -----------------------\n")
  cat("    Medida F:", F_M,"      Valor eps:", C_M$eps,"      Valor minPts:", C_M$minPts,"\n")
  cat("\n\t\tGracias por usar este algoritmo :)\n")
  cat("---------------------------------------------------------------------\n")
  
  # Se crea una lista que contendrá datos importantes 
  return(C_M)
}

#### PUREZA ####
# Funcion que regresa el vector de puridad de clusters
# Recibe matriz de tabla de contingencia
# Regresa vector de pureza 
Purity_vec <- function(Tabla){
  pur_vec <- c()
  
  # Ciclo que recorre todos los clusters
  for (i in 1:length(Tabla[,1])){
    # Pureza del cluster i
    pur_vec <- c(pur_vec, max(Tabla[i,])/sum(Tabla[i,]))
  }
  pur_vec <- NaNOut(pur_vec)
  return(pur_vec)
}
#### RECALL ####
recall_vec <- function(Tabla){
  rec_vec <- c()
  
  # Ciclo que recorre todos los clusters
  for (i in 1:length(Tabla[,1])){
    # Pureza del cluster i
    ji <- which.max(Tabla[i,])
    rec_vec <- c(rec_vec, Tabla[i,ji]/sum(Tabla[,ji]))
  }
  
  rec_vec <- NaNOut(rec_vec)
  return(rec_vec)
}


#### MEDIDAD F v.2 ####
# I recieve: Tabla de contingencia
# You recieve: clusterización optima basada en densidad
medida_f2 <- function(Tabla){
  suma <- 0
  for(i in 1:length(Tabla[,1])){
    ji <- which.max(Tabla[i,])
    suma <- suma + (2*Tabla[i,ji])/(sum((Tabla[i,])) + sum(Tabla[,ji]))
  }

  suma <- 1/length(Tabla[,1])*suma
  return(suma)
}

#### MEDIDA BASADA EN ENTROPIA (ALPHA) ####
# Recibe tabla de contingencia y cantidad de datos
# regresa medida de entropia
medida_entropia <- function(Tabla, n){
  ECn <- length(Tabla[,1])
  RCn <- length(Tabla[1,])
  P_Matrix <- Prob_matrix(Tabla)
  entropia <- vector("numeric", length = ECn)
  E <- 0
  
  for(i in 1:ECn){
    for(j in 1:RCn){
      if(!is.infinite(log2(P_Matrix[i,j]))){
        entropia[i] <- entropia[i] + P_Matrix[i,j]*log2(P_Matrix[i,j])
      }
    }
  }
  
  # Se calcula la entropia condicional para la clusterizacion
  for(i in 1:ECn){
    E <- E + sum(Tabla[i,])*entropia[i]/n
  }
  
  return(-E)
}

# Recibe una tabla de contingencia y la cantidad de datos
# Regresa matriz de probabilidad de interseccion de clusters
Prob_matrix <- function(Tabla){
  row <- length(Tabla[,1])
  col <- length(Tabla[1,])
  P_M <- matrix(data=NA, nrow = row, ncol = col)
  
  for(i in 1:row){
    for(j in 1:col){
      P_M[i,j] <- Tabla[i,j]/sum(Tabla[i,])
    }
  }
  
  return(P_M)
} 






















#### ----  BASURERO ---- ####
#### MEDIDA F v.1 ###
# Recibe una tabla de contingencia
# Regresa un valor numérico entre 0 y 1
medida_f <- function(Tabla){
  pv <- Purity_vec(Tabla)
  rv <- recall_vec(Tabla)
  
  return((1/length(Tabla[1,]))*sum((2*pv*rv)/(pv+rv)))
}
#### OMITIR NaN ###
NaNOut <- function(vec){
  for(i in 1:length(vec)){
    if(is.nan(vec[i])){
      vec[i] <- 0
    }
  }
  return(vec)
}