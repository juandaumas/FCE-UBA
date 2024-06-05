# ---- Ejercicio 1 - Primer Parcial - Juan Daumas - 900496

rm(list=ls())
library(tidyverse)
library(ggplot2)


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

S = 75
K = 78
Vol = 0.25
Q = 0.12
TLR = 0.06 #Continua
t = 6/12 # meses/año
n = 2 #pasos del árbol

Valuacion = valuacion_derivado(S0 = S,K = K,tlr = TLR,tasa = 0,vol = Vol,pasos = n,t = t,tipo_tasa = "continua",tipo_derivado = "put")
#A arbol binomial de dos pasos:
Valuacion$precios_subyacente
#B prima de la opción
Valuacion$val_derivado[1,1]
