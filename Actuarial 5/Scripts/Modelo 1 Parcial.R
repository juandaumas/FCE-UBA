# Nombre y numero de Registro

# Ejercicio Modelo Parcial
rm(list=ls())

# Librerias

library(ggplot2())
## Funciones #####

valuacion_derivado <- function(S0,K,tlr,tasa,t,vol,pasos,u = NULL, d = NULL,tipo_tasa = "continua",tipo_derivado = "put"){
  # DATO si la TLR no es anual la parte de la FD estaria mal.
  
  n <- pasos
  P <- NULL
  if ( is.null(u) || is.null(d)){
    u <- exp(vol*sqrt(t/n))
    d <- 1/u  
  } else {
    u = u
    d = d
  }
  if (tipo_tasa == "continua"){
    P <- (exp((tlr-tasa)*t/n)-d)/(u-d)
    FD <- exp(-tlr*t/n)
    FD_sub <- exp(-t/n*tasa)
  } else if(tipo_tasa == "efectiva"){
    P <- (((1+tlr)/(1+tasa))-d)/(u-d)  
    FD <- ((1+tlr)^(-1)) #La tasa ya tiene que estar hecha efectiva con la capitalizacion. Ej: si TNA Cap Trim -> TLR = TNA/4
    FD_sub <- (1+tasa)^(-t/n)
  }
  
  subyacente_precios <- matrix(0, n + 1, n + 1)
  subyacente_precios[1, 1] <- S0
  
  for (i in 1:(n+1)) {
    for (j in 2:(n+1)) {
      subyacente_precios[i, j] <- subyacente_precios[i, j-1] * u
      subyacente_precios[j, j] <- subyacente_precios[j-1, j-1] * d
    }
  }
  
  val_derivado <- matrix (0, n + 1, n + 1)
  
  for (i in (n+1):1){
    if (tipo_derivado == "put") {val_derivado[i,n+1] <- max(0,K - subyacente_precios[i,n+1])} else if(tipo_derivado == "call") {val_derivado[i,n+1] <- max(0, subyacente_precios[i,n+1] - K)}
  }
  
  for (g in n:1){
    for (j in g:1){
      val_derivado[j,g] <- (P * val_derivado[j,g+1] + (1-P) * val_derivado[j+1,g+1] ) * FD
      
    }
  }
  deltas <- matrix(0,n,n)
  for (h in 1:n) {
    for (k in 1:n) {
      deltas[h,k] <- (val_derivado[h,k+1] -val_derivado[h+1,k+1])/( subyacente_precios[h,k+1]- subyacente_precios[h+1,k+1])
    }
  }
  deltas[2,1] <- 0
  deltas_descontados <- deltas*FD_sub
  
  return(list(U = u,D = d,P = P, precios_subyacente = subyacente_precios, 
              val_derivado = val_derivado,deltas = deltas,
              deltas_descontados = deltas_descontados))
}

Arbitraje <- function(Precios_subyacente,prima_mercado, prima_teorica,deltas,deltas_desc,tlr,t,n, tipo_subyacente, tipo_derivado,camino_precio){
  # Hay que cambiar los FC dependiendo como den la tasa
  # Si tlr -> continua y anual sin capitalizacion estaria ok.
  # Si tlr -> efectiva con capitalizacion. Modificar (1+tlr)^1 o (1+tlr)^2
  S0 <- Precios_subyacente[1,1]
  Delta_1 <- deltas[1,1]
  Delta_1_desc <- deltas_desc[1,1]
  
  if(camino_precio == "U-U") {
    S1 <- Precios_subyacente[1,2]
    S2 <- Precios_subyacente[1,3]
    Delta_2 <- deltas[1,2]
    Delta_2_desc <- deltas_desc[1,2]
  } 
  else if(camino_precio == "U-D") {
    S1 <- Precios_subyacente[1,2]
    S2 <- Precios_subyacente[2,3]
    Delta_2 <- deltas[1,2]
    Delta_2_desc <- deltas_desc[1,2]
  } 
  else if(camino_precio == "D-U") {
    S1 <- Precios_subyacente[2,2]
    S2 <- Precios_subyacente[2,3]
    Delta_2 <- deltas[2,2]
    Delta_2_desc <- deltas_desc[2,2]
  } 
  else if(camino_precio == "D-D") {
    S1 <- Precios_subyacente[2,2]
    S2 <- Precios_subyacente[3,3]
    Delta_2 <- deltas[2,2]
    Delta_2_desc <- deltas_desc[2,2]
  } else {
    stop()
  }
  columnas <- c("Cantidad","Flujo_de_Fondo")
  filas <- c("Derivado","Subyacente","Deuda_1","Deuda_2","Inversion_1","Inversion_2","Sumatoria")
  Dif_prima <- NULL
  t1 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                          dimnames = list(filas, columnas)))
  t2 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                          dimnames = list(filas, columnas)))
  t3 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                          dimnames = list(filas, columnas)))
  Replica <- list(Dif_prima = Dif_prima,T1 = t1, T2 = t2, T3 = t3)
  
  if (tipo_subyacente == "accion sin rendimiento") {
    if (prima_mercado < prima_teorica & tipo_derivado == "call") {
      cat("Compro Call - Vendo subyacente - Invierto restante\n)")
      Replica$Dif_prima <- prima_teorica - prima_mercado
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- -Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Replica$T1["Subyacente", "Cantidad"] * S0
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"],5)
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2 - Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  -Replica$T1["Subyacente", "Cantidad"] + Replica$T2["Subyacente","Cantidad"]
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- -Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
    }
    else if (prima_mercado < prima_teorica & tipo_derivado == "put"){
      cat("Compro Put - Compro subyacente - Tomo deuda)")
      Replica$Dif_prima <- prima_teorica - prima_mercado
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1_desc
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Replica$T1["Subyacente", "Cantidad"] * S0
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2 - Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(K-S2,0)
      Replica$T3["Subyacente","Cantidad"] <-  -Replica$T1["Subyacente", "Cantidad"] + Replica$T2["Subyacente","Cantidad"]
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- -Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    }
    else if (prima_mercado > prima_teorica & tipo_derivado == "call") {
      cat("Vendo Call - Compro subyacente - Tomo deuda)")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Delta_1 * S0
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2 - Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Inversion_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- -max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  Replica$T1["Subyacente", "Cantidad"] + Replica$T2["Subyacente","Cantidad"]
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    }
    else if (prima_mercado > prima_teorica & tipo_derivado == "put"){
      cat("Vendo Put - Vendo subyacente - Invierto restante\n")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Delta_1 * S0
      Replica$T1["Inversion_1", "Cantidad"] <- 1
      Replica$T1["Inversion_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2 - Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- Replica$T2["Deuda_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Inversion_1","Flujo_de_Fondo"]  <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Inversion_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- -max(K-S2,0)
      Replica$T3["Subyacente","Cantidad"] <-  Replica$T1["Subyacente", "Cantidad"] + Replica$T2["Subyacente","Cantidad"]
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
    } 
    else {stop()}
  }
  else if (tipo_subyacente == "accion con rendimiento") {
    if (prima_mercado < prima_teorica & tipo_derivado == "call") {
      cat("Compro Call - Vendo subyacente - Invierto restante)")
      Replica$Dif_prima <- prima_teorica - prima_mercado
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- -Delta_1_desc
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Replica$T1["Subyacente", "Cantidad"] * S0
      Replica$T1["Inversion_1", "Cantidad"] <- 1
      Replica$T1["Inversion_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2_desc - Delta_1
      if(Delta_2 - Delta_1 > 0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Inversion_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Inversion_1","Flujo_de_Fondo"]  <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- Replica$T2["Deuda_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- -Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    } #Ok
    else if (prima_mercado < prima_teorica & tipo_derivado == "put"){ 
      cat("Compro Put - Compro subyacente - Tomo deuda)")
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1_desc
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- Replica$T1["Subyacente", "Cantidad"] * S0
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2_desc - Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(K-S2,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- -Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    } # OK
    else if (prima_mercado > prima_teorica & tipo_derivado == "call") {
      cat("Vendo Call - Compro subyacente - Tomo deuda)")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1_desc
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- -Replica$T1["Subyacente", "Cantidad"] * S0
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_2_desc - Delta_1
      if(Delta_2 - Delta_1 > 0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- Replica$T2["Deuda_2","Flujo_de_Fondo"] * (exp(tlr*t/n))
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Cantidad"] * S1
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * (exp(tlr*t))
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * (exp(tlr*t/n))
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- -max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * S2
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    }
    else if (prima_mercado > prima_teorica & tipo_derivado == "put"){
      cat("Vendo Put - Vendo subyacente - Invierto restante")
      Replica$T1["Derivado", "Cantidad"]
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      
    } 
    else {stop()
    }
  } 
  else if (tipo_subyacente == "futuro") {
    if (prima_mercado < prima_teorica & tipo_derivado == "call") {
      cat("Compro Call - Vendo subyacente - Tomo Deuda\n)")
      Replica$Dif_prima <- prima_teorica - prima_mercado
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- 0 #Futuro no paga para entrar. Daily settlement
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S0-S1)
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * exp(tlr*t/n)
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * exp(tlr*t)
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S0-S1)
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * exp(tlr*t)
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * exp(tlr*t/n)
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- -Replica$T3["Subyacente","Cantidad"] * (S2-S1)
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    }
    else if (prima_mercado < prima_teorica & tipo_derivado == "put"){
      cat("Compro Put - Compro subyacente - Tomo deuda)")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- -Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- 0 #Futuro no paga para entrar. Daily settlement
      Replica$T1["Deuda_1", "Cantidad"] <- 1
      Replica$T1["Deuda_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S0-S1)
        Replica$T2["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T2["Inversion_1","Flujo_de_Fondo"] * exp(tlr*t/n)
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * exp(tlr*t)
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S0-S1)
        Replica$T3["Deuda_1","Flujo_de_Fondo"]  <- -Replica$T1["Deuda_1", "Flujo_de_Fondo"] * exp(tlr*t)
        Replica$T2["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_2","Flujo_de_Fondo"] <- -Replica$T2["Deuda_2","Flujo_de_Fondo"] * exp(tlr*t/n)
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- max(K-S2,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * (S1-S2)
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    } #OK
    else if (prima_mercado > prima_teorica & tipo_derivado == "call") {
      cat("Vendo Call - Compro subyacente - Invierto)")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- 0 #Futuro no paga para entrar. Daily settlement
      Replica$T1["Inversion_1", "Cantidad"] <- 1
      Replica$T1["Inversion_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S1-S0)
        Replica$T2["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Inversion_2","Flujo_de_Fondo"] * (1+tlr)^1
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (1+tlr)^2
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S1-S0)
        Replica$T3["Inversion_1","Flujo_de_Fondo"]  <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * (1+tlr)^2
        Replica$T2["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Deuda_1","Flujo_de_Fondo"] * (1+tlr)^1
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- -max(S2-K,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * (S2-S1)
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    } #OK
    else if (prima_mercado > prima_teorica & tipo_derivado == "put"){
      cat("Vendo Put - Vendo subyacente - Invierto restante")
      Replica$Dif_prima <- prima_mercado - prima_teorica
      
      Replica$T1["Derivado", "Cantidad"] <- 1
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      Replica$T1["Subyacente", "Cantidad"] <- Delta_1
      Replica$T1["Subyacente", "Flujo_de_Fondo"] <- 0 #Futuro no paga para entrar. Daily settlement
      Replica$T1["Inversion_1", "Cantidad"] <- 1
      Replica$T1["Inversion_1", "Flujo_de_Fondo"] <- -sum(Replica$T1["Derivado", "Flujo_de_Fondo"],Replica$T1["Subyacente", "Flujo_de_Fondo"])
      Replica$T1["Sumatoria", "Flujo_de_Fondo"] <- sum(Replica$T1[,"Flujo_de_Fondo"])
      
      Replica$T2["Subyacente","Cantidad"] <- Delta_1
      if(Delta_2 - Delta_1>0){
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S1-S0)
        Replica$T2["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Deuda_1","Flujo_de_Fondo"] <- -Replica$T2["Deuda_1","Flujo_de_Fondo"] * exp(tlr*t/n)
        Replica$T3["Inversion_1","Flujo_de_Fondo"] <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * exp(tlr*t)
      } else {
        Replica$T2["Subyacente","Flujo_de_Fondo"] <- Replica$T2["Subyacente","Cantidad"] * (S1-S0)
        Replica$T3["Inversion_1","Flujo_de_Fondo"]  <- -Replica$T1["Inversion_1", "Flujo_de_Fondo"] * exp(tlr*t)
        Replica$T2["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Subyacente","Flujo_de_Fondo"]
        Replica$T3["Inversion_2","Flujo_de_Fondo"] <- -Replica$T2["Inversion_2","Flujo_de_Fondo"] * exp(tlr*t/n)
      }
      
      Replica$T3["Derivado","Cantidad"] <- Replica$T1["Derivado", "Cantidad"]
      Replica$T3["Derivado","Flujo_de_Fondo"] <- -max(K-S2,0)
      Replica$T3["Subyacente","Cantidad"] <-  Delta_2
      Replica$T3["Subyacente","Flujo_de_Fondo"] <- Replica$T3["Subyacente","Cantidad"] * (S2-S1)
      
      Replica$T3["Sumatoria","Flujo_de_Fondo"] <- sum(Replica$T3[,"Flujo_de_Fondo"])
      
    } #OK 
    else {stop()}
  }
  else if (tipo_subyacente == "moneda extranjera") {
    if (prima_mercado < prima_teorica & tipo_derivado == "call") {
      cat("Compro Call - Vendo subyacente - Invierto restante)")
      Replica$T1["Derivado", "Cantidad"]
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      
    } else if (prima_mercado < prima_teorica & tipo_derivado == "put"){
      cat("Compro Put - Compro subyacente - Tomo deuda)")
      Replica$T1["Derivado", "Cantidad"]
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      
    } else if (prima_mercado > prima_teorica & tipo_derivado == "call") {
      cat("Vendo Call - Compro subyacente - Tomo deuda)")
      Replica$T1["Derivado", "Cantidad"]
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      
    } else if (prima_mercado > prima_teorica & tipo_derivado == "put"){
      cat("Vendo Put - Vendo subyacente - Invierto restante")
      Replica$T1["Derivado", "Cantidad"]
      Replica$T1["Derivado", "Flujo_de_Fondo"] <- Replica$T1["Derivado", "Cantidad"] * prima_mercado
      
    } else {stop()}
  }
  return(Replica)
}

### Ejercicio 1

S = 70
K = 68
Vol = 0.20
Q = 0.10
TLR = 0.05 #Continua
t = 0.5
n = 2

# El rendimiento del activo es IRRELEVANTE.

Valuacion <- valuacion_derivado(S0 = S,K = K,tlr = TLR,tasa = 0,t = t,
                                     vol = Vol,pasos = n,tipo_tasa = "continua",tipo_derivado = "call")
Valuacion
#A - Arbol Binomial  de dos pasos. Precios
Valuacion$precios_subyacente
#B - Prima
Valuacion$val_derivado[1,1]

#C 
Estrategia <- Arbitraje(Precios_subyacente = Valuacion$precios_subyacente, prima_mercado = 6.5,prima_teorica = Valuacion$val_derivado[1,1],
          deltas = Valuacion$deltas,deltas_desc = Valuacion$deltas_descontados,
          tlr = TLR,t = t,n = 2,tipo_subyacente = "accion sin rendimiento",
          tipo_derivado = "call",camino_precio = "U-U")
Estrategia
# Valido si funciono la estategia:
Estrategia$Dif_prima
Estrategia$T3$Flujo_de_Fondo[7] * exp(-TLR*t)

# Debe dar 0 o proximo
round(Estrategia$Dif_prima - Estrategia$T3$Flujo_de_Fondo[7] * exp(-TLR*t),5)

# Ejercicio 2
rm(list=ls())

#Verificacion limites de call/put
S <- 33
K1 <- 30
K2 <- 35
K3 <- 40

C_prima1 <- 3
C_prima2 <- 1.5
C_prima3 <- 0.75
P_prima1 <- 1.75
P_prima2 <- 2.50
P_prima3 <-5
t <- 0.25
TLR <- 0.12
# Verifico posibilidades de arbitraje
C_prima1 >= (S- K1*exp(-TLR*0.25))
C_prima2 >= (S- K2*exp(-TLR*0.25))
C_prima3 >= (S- K3*exp(-TLR*0.25))

P_prima1 >= (K1*exp(-TLR*0.25)-S)
P_prima2 >= (K2*exp(-TLR*0.25)-S)
P_prima3 >= (K3*exp(-TLR*0.25)-S)

# A 
Call_1_K <- 30 # Long (ST-K)
Call_1_P <- 3
Call_2_K <- 35 # Short -(ST-K)
Call_2_P <- 1.5

ST_Vector <- seq(20, 45, by = 1)


payoff_long <- pmax(ST_Vector - Call_1_K, 0) - Call_1_P
payoff_short <- -pmax(ST_Vector - Call_2_K, 0) + Call_2_P

Pay_off_estrategia <- payoff_long + payoff_short

grafico_cartera_payoff_1 <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Pay Off estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bull Spread de Calls", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Pay Off estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "black"), 
                     labels=c("Pay Off estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff_1 + geom_abline(slope = 0, linetype = "dashed", color = "grey")

# B
Put_1_K <- 30 # es Short -(K-ST)
Put_1_P <- 1.75 
Put_2_K <- 35 # es Long (K-ST)
Put_2_P <- 2.5


ST_Vector <- seq(20, 45, by = 1)

payoff_short <- -pmax(Put_1_K - ST_Vector, 0) + Put_1_P
payoff_long <- pmax(Put_2_K - ST_Vector, 0) - Put_2_P

Pay_off_estrategia <- payoff_short + payoff_long

grafico_cartera_payoff_2 <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "black"), 
                     labels=c("Estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff_2 + geom_abline(slope = 0, linetype = "dashed", color = "grey")

# C
rm(list=ls())

  # En el parcial habria que chequer todos los precios teoricos de cada derivado.

S = 33
K = 30
prima_mercado = 3
t = 0.25
TLR <- 0.12
St <- 33*exp(0.12*0.25)

# c =< S0 - K*e^-rt

c <- 33 - 30 * exp(-0.12*0.25)
# Vendo prima teorica - Compro call en el mercado
dif <- c-3
columnas <- c("Cantidad","Flujo_de_Fondo")
filas <- c("Derivado","Subyacente","Deuda_1","Deuda_2","Inversion_1","Inversion_2","Sumatoria")
Dif_prima <- NULL
t1 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))
t2 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))

t1["Derivado","Cantidad"] <- 1
t1["Derivado","Flujo_de_Fondo"] <- -prima_mercado
t1["Subyacente","Cantidad"] <- -1
t1["Subyacente","Flujo_de_Fondo"] <- S
t1["Inversion_1","Flujo_de_Fondo"] <- -(-c+S)
t1["Sumatoria","Flujo_de_Fondo"] <- sum(t1$Flujo_de_Fondo[1:6])

t2["Derivado","Cantidad"] <- 1
t2["Derivado","Flujo_de_Fondo"] <- max(St-K,0)
t2["Subyacente","Cantidad"] <- -1
t2["Subyacente","Flujo_de_Fondo"] <- -St
t2["Inversion_1","Flujo_de_Fondo"] <- -t1["Inversion_1","Flujo_de_Fondo"] * (exp(TLR*t))
t2["Sumatoria","Flujo_de_Fondo"] <- sum(t2$Flujo_de_Fondo[1:6])

df <- data.frame(t1,t2)
df
# C
rm(list=ls())

# En el parcial habria que chequer todos los precios teoricos de cada derivado.

S = 33
K = 40
prima_mercado = 5
t = 0.25
TLR <- 0.12
St <- 33*exp(0.12*0.25)

# P =< K*e^-rt - S0 

p <- K * exp(-0.12*0.25) - 33  
# Vendo prima teorica - Compro put en el mercado
dif <- p-prima_mercado
columnas <- c("Cantidad","Flujo_de_Fondo")
filas <- c("Derivado","Subyacente","Deuda_1","Deuda_2","Inversion_1","Inversion_2","Sumatoria")
Dif_prima <- NULL
t1 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))
t2 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))

t1["Derivado","Cantidad"] <- 1
t1["Derivado","Flujo_de_Fondo"] <- -prima_mercado
t1["Subyacente","Cantidad"] <- -1
t1["Subyacente","Flujo_de_Fondo"] <- -S
t1["Deuda_1","Flujo_de_Fondo"] <- (p+S)
t1["Sumatoria","Flujo_de_Fondo"] <- sum(t1$Flujo_de_Fondo[1:6])

t2["Derivado","Cantidad"] <- 1
t2["Derivado","Flujo_de_Fondo"] <- max(K-St,0)
t2["Subyacente","Cantidad"] <- -1
t2["Subyacente","Flujo_de_Fondo"] <- St
t2["Deuda_1","Flujo_de_Fondo"] <- -t1["Deuda_1","Flujo_de_Fondo"] * (exp(TLR*t))
t2["Sumatoria","Flujo_de_Fondo"] <- sum(t2$Flujo_de_Fondo[1:6])

df <- data.frame(t1,t2)


# FUNCIONA - Solo que recibo la diferencia en t1


#D
rm(list=ls())

S <- 33
St <- 33*exp(0.12*0.25)
K <- 40
TLR <- 0.12
t = 0.25
Prima_mercado <- 5
Prima_teorica <- K*exp(-TLR*t) - S

Dif_prima <- Prima_teorica - Prima_mercado

# Prima Teorica > Prima Mercado ---> Vendo la teorica compro la de mercado

columnas <- c("Cantidad","Flujo_de_Fondo")
filas <- c("Derivado","Subyacente","Deuda_1","Deuda_2","Inversion_1","Inversion_2","Sumatoria")

t1 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))
t2 <- data.frame(matrix(0, nrow = length(filas), ncol = length(columnas),
                        dimnames = list(filas, columnas)))

t1["Derivado","Cantidad"] <- 1
t1["Derivado","Flujo_de_Fondo"] <- -Prima_mercado
t1["Subyacente","Cantidad"] <- 1
t1["Subyacente","Flujo_de_Fondo"] <- -S
t1["Deuda_1","Flujo_de_Fondo"] <- (Prima_mercado+S)
t1["Sumatoria","Flujo_de_Fondo"] <- sum(t1$Flujo_de_Fondo[1:6])

t2["Derivado","Cantidad"] <- 1
t2["Derivado","Flujo_de_Fondo"] <- max(K-St,0)
t2["Subyacente","Cantidad"] <- -1
t2["Subyacente","Flujo_de_Fondo"] <- St
t2["Deuda_1","Flujo_de_Fondo"] <- -t1["Deuda_1","Flujo_de_Fondo"]*(exp(TLR*t))
t2["Sumatoria","Flujo_de_Fondo"] <- sum(t2$Flujo_de_Fondo[1:6])

##  Verifique la paridad call-put si S = 33, las opciones vencen en 3 meses y la tlr es de 0.12. 
##  Considere todos los precios de ejercicio de la tabla. Si no se verifica la parida realice el arbitraje correspondiente
rm(list=ls())
# Call_1 y Prima_1
# C + S0 = P + K*exp(-r*t)

#  1.75 + 33 ?=? 3 + 30 * exp(-r*t)

S0 <- 33
t <- 3/12
K <- 30
Prima_Call <- 3
Prima_Put <- 5
TLR <- 0.12

put_parity <- 1.75+33
call_parity <- 3+30*exp(-TLR*t)
diferencia <- call_parity - put_parity

if (isFALSE(put_parity == call_parity) ) {
  if (call_parity > put_parity) {
    paste("Vendo Call - Vendo Subyacente - Compro Put - Invierto")
  } else {
    paste("Compro Call - Compro Subyacente - Vendo Put - Tomo Deuda")
  }
}
filas <- c("Call","Put","Subyacente","Deuda","Inversion")
columnas <- c("Cantidad","Flujo_de_fondo")

df <- data.frame(matrix(0,nrow=5,ncol=2,dimnames = list(filas,columnas)))
df["Call","Cantidad"] <- 1
df["Call","Flujo_de_fondo"] <- -c
df["Put","Cantidad"] <- -1
df["Put","Flujo_de_fondo"] <- p
df["Subyacente","Cantidad"] <- 1
df["Subyacente","Flujo_de_fondo"] <- -S0
df["Deuda","Cantidad"] <-
df["Deuda","Flujo_de_fondo"] <-
df["Inversion","Cantidad"] <-
df["Inversion","Flujo_de_fondo"] <- 1

# Ejercicio 3
rm(list=ls())

# Dado que son TNA para el plazo, el FDi es (1+TNA*i/12)^-1
# Si t fueran porciones de año 1/12 , 2/12 -> FD es (1+TNA*t)^-1
# FDt <- (1+TEA)^-t
# Si fueran TNA con cap continua FD = exp(-r*t)

# A
Mes <- seq(1,12)
TNA <- c(0.0405,0.045,0.0475,0.05,0.0525,0.055,0.0575,0.06,0.0625,0.065,0.0675,0.07)

FD_ <- (1+TNA/12)^-(Mes/12)
FD_2 <- (1+TNA/12)^-Mes #puedo usarla solo si es Capitalizacion anual para cada periodo
FD <- (1+TNA*Mes/12)^-1
data.frame(Mes,TNA,FD)

VN <- 100
TLR <- 0.12 #Pagos Mensuales
TLR_Mensual <- TLR/12
Vto <- 1

Flujos <- rep(VN*TLR_Mensual,12)
Flujos[12] <- Flujos[12]+VN
Flujos

Precio_bono <- sum(FD*Flujos)
Precio_bono

# B
VN <- 50000000

  #FC(0,1)*FC(1,9) = FC(0,10)
  # (1+TNA*mes[1]/12) * FC(1,9) =  (1+TNA*mes[10]/12)
  # 1/FD1 * FC = 1/FD9
Tasa_forward <- (FD[1] / FD[10] - 1) * 12/9

# B
tasa_fra <- Tasa_forward
LIBOR <- 0.0825
tasa_prestamo <- LIBOR + 0.0125
plazo <- 9/12
# Demostracion del costo


Flujo_fra <- VN*(LIBOR-tasa_fra)*plazo*(1+LIBOR*plazo)^-1
prestamo <- VN - Flujo_fra
Devuelvo <- prestamo * (1+tasa_prestamo*plazo)

Costo <- (((Devuelvo)/VN)-1) * 12/9
Costo

# C
Trimestres <- seq(1,4)
Mes_trim <- c(3,6,9,12)
TNA_trim <- c(TNA[3],TNA[6],TNA[9],TNA[12])
FD_trim <- (1+TNA_trim*Trimestres/4)^-1
FD_2 <-(1+TNA_trim/4)^-(Mes_trim/Mes_trim[1])
df_trim <- data.frame(Trimestres,TNA_trim,FD_trim)

tasa_swap <- (1-FD_trim[4])/sum(FD_trim)

tasa_swap*4

# Ejercicio 4
rm(list=ls())

Rendimiento_acciones <- 0.2
Desvio_acciones <- 0.18
Rendimiento_bonos <- 0.12
Desvio_bonos <- 0.15

ponderacion_activo <-seq(0,1,by = 0.001)
ponderacion_bono <- 1-ponderacion_activo

Retorno_portafolio <- ponderacion_activo * Rendimiento_acciones + ponderacion_bono * Rendimiento_bonos

rho_1 = -1
rho_2 = -0.5
rho_3 = 0 
rho_4 = 0.5
rho_5 = 1

cov_1 <- rho_1 * Desvio_acciones * Desvio_bonos
cov_2 <- rho_2 * Desvio_acciones * Desvio_bonos
cov_3 <- rho_3 * Desvio_acciones * Desvio_bonos
cov_4 <- rho_4 * Desvio_acciones * Desvio_bonos
cov_5 <- rho_5 * Desvio_acciones * Desvio_bonos


Varianza_1_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_1
Desvio_1_portafolio <- sqrt(Varianza_1_portafolio)

Varianza_2_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_2
Desvio_2_portafolio <- sqrt(Varianza_2_portafolio)

Varianza_3_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_3
Desvio_3_portafolio <- sqrt(Varianza_3_portafolio)

Varianza_4_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_4
Desvio_4_portafolio <- sqrt(Varianza_4_portafolio)

Varianza_5_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_5
Desvio_5_portafolio <- sqrt(Varianza_5_portafolio)


Analisis_portafolio <- data.frame(
  Pond_activo = ponderacion_activo,
  Pond_bono = ponderacion_bono,
  Rendimiento_portafolio = Retorno_portafolio,
  Desvio_p_1 = Desvio_1_portafolio,
  Desvio_p_2 = Desvio_2_portafolio,
  Desvio_p_3 = Desvio_3_portafolio,
  Desvio_p_4 = Desvio_4_portafolio,
  Desvio_p_5 = Desvio_5_portafolio
)
Analisis_portafolio

ggplot(Analisis_portafolio) + geom_point(aes(x=Desvio_p_1, y=Rendimiento_portafolio,color="Rho = -1")) +
  geom_point(aes(x=Desvio_p_2, y=Rendimiento_portafolio,color="Rho = -0.5"))+
  geom_point(aes(x=Desvio_p_3, y=Rendimiento_portafolio,color="Rho = 0"))+
  geom_point(aes(x=Desvio_p_4, y=Rendimiento_portafolio,color="Rho = 0.5"))+
  geom_point(aes(x=Desvio_p_5, y=Rendimiento_portafolio,color="Rho = 1"))+
  scale_color_discrete() + 
  ggtitle("Riesgo-Retorno del Portafolio",subtitle = "Analisis para distintos coeficientes de correlacion") +
  xlab("Desviación Estándar") + ylab("Rendimiento") +labs(color = "Coeficientes de Correlación") +
  theme_classic()

# B
  
TLR <- 0.06

#Cartera optima por Rendimiento

ratio_sharpe <- (Analisis_portafolio$Rendimiento_portafolio - TLR)/Analisis_portafolio$Desvio_p_4

indice_maximo <- which.max(ratio_sharpe)

Cantidad_activo_optima <- Analisis_portafolio$Pond_activo[indice_maximo]
Cantidad_bono_optima <- Analisis_portafolio$Pond_bono[indice_maximo]
Rendimiento_cartera_optima <-Analisis_portafolio$Rendimiento_portafolio[indice_maximo]
Desvio_cartera_optima <- Analisis_portafolio$Desvio_p_4[indice_maximo]

Pendiente_CAL <- max(ratio_sharpe)
Portafolio_rho4 <- NULL
Portafolio_rho4 <- data.frame(Analisis_portafolio[,c(1:3,7)])

ggplot(data = Portafolio_rho4, aes(x = Desvio_p_4, y = Rendimiento_portafolio)) +
  geom_point(aes(color = "Ponderaciones portafolio"),size = 0.1) +
  geom_point(aes(x = Desvio_cartera_optima, y = Rendimiento_cartera_optima, color = "Portafolio Optimo")) +
  geom_abline(slope = Pendiente_CAL, intercept = TLR,color = "red") +
  labs(title = "Grafico Portafolio Optimo", subtitle = "con Capital Allocation Line", x = "Desvio", y = "Rendimiento") +
  scale_color_manual(values = c("Ponderaciones portafolio" = "black", "Portafolio Optimo" = "green")) +
  xlim(0.1, 0.25) + ylim(0, 0.25) +
  theme_classic() + guides(color = guide_legend(title = "Leyenda")) 


