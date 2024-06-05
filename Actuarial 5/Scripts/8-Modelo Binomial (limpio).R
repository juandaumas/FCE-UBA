## Modelo Binomial en limpio

rm(list = ls())

## Primer Ejemplo ########

tlr <- 0.26/4 # TNA cap trim
tasa <- 0.26/4 # Por ser futuro
vol <- 0.2
S0 <- 27
K <- 28
t <- 0.5
pasos <- 10 # cantidad de pasos

############ Funciones solitarias #######################
valuacion_subyacente <- function(S0,K,tlr,tasa,t,vol,pasos,tipo_tasa = "continua"){
  
  n <- pasos

  u <- exp(vol*sqrt(t/n))
  d <- 1/u
  
  if (tipo_tasa == "continua"){
    P <- (exp((tasa-tlr)*t)-d)/(u-d)
  } else if(tipo_tasa == "efectiva"){
    P <- (((1+tasa)/(1+tlr))-d)/(u-d)  
  }
  precios <- matrix(0, n + 1, n + 1)
  precios[1, 1] <- S0
  
  for (i in 1:(n+1)) {
    for (j in 2:(n+1)) {
      precios[i, j] <- precios[i, j-1] * u
      precios[j, j] <- precios[j-1, j-1] * d
    }
  }
  return(precios)
}

Precios_subyacente <- valuacion_subyacente(S0,K,tasa,tlr,t,vol,pasos)

valuar_derivado <- function(subyacente_precios,K,tlr,P,pasos,tipo_derivado = "put"){
  n <- pasos
  val_derivado <- matrix (0, n + 1, n + 1)
  
  FD <- ((1+tlr)^-1)
  
  for (i in (n+1):1){
    if (tipo_derivado == "put") {val_derivado[i,n+1] <- max(0,K - subyacente_precios[i,n+1])} else if(tipo_derivado == "call") {val_derivado[i,n+1] <- max(0, subyacente_precios[i,n+1] - K)}
  }
  
  for (g in n:1){
    for (j in g:1){
      val_derivado[j,g] <- (P * val_derivado[j,g+1] + (1-P) * val_derivado[j+1,g+1] ) * FD
    }
  }
  return(val_derivado)
}

valuar_derivado(Precios_subyacente,K,tlr,P,pasos,"put")



############ FUNCION CORRECTA #######################
######## Formula general ######

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

valuacion_derivado(S0 = S0, K = K,tlr = tlr,tasa = tasa,t = t,vol = vol,pasos = pasos,tipo_tasa = "efectiva",tipo_derivado = "put")

# EJEMPLO POWERP POINT MODELO BINOMIAL

S <- 40
K <- 40
tlr <- 0.05
t <- 0.5
n <- 2
vol <- 0.336 # ( u = 1.1 ; d = 0.9)

# PUT Power Point Dario
valuacion <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = 0,t = t,vol = vol,pasos = n,u = 1.1,d = 0.9,tipo_tasa = "continua",tipo_derivado = "put")

valuacion_2 <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = 0,t = t,vol = vol,pasos = n,u = 1.1,d = 0.9,
                                tipo_tasa = "continua",tipo_derivado = "call")


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
      Replica$Dif_prima <- prima_teorica - prima_mercado
      
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

Arbitraje(Precios_subyacente = valuacion$precios_subyacente,
          prima_mercado = 1.5,prima_teorica = valuacion$val_derivado[1,1],deltas = valuacion$deltas,
          deltas_desc = valuacion$deltas_descontados,
          tlr = tlr , t = t,n=n,
          tipo_subyacente = "accion sin rendimiento",tipo_derivado = "call",camino_precio = "U-U")

Arbitraje(Precios_subyacente = valuacion_2$precios_subyacente,
          prima_mercado = 1.5,prima_teorica = valuacion_2$val_derivado[1,1],deltas = valuacion_2$deltas,
          deltas_desc = valuacion$deltas_descontados,
          tlr = tlr , t = t,n=n,
          tipo_subyacente = "accion sin rendimiento",tipo_derivado = "call",camino_precio = "D-D")

Arbitraje(Precios_subyacente = valuacion_2$precios_subyacente,
          prima_mercado = 3,prima_teorica = valuacion_2$val_derivado[1,1],deltas = valuacion_2$deltas,
          deltas_desc = valuacion$deltas_descontados,
          tlr = tlr , t = t,n=n,
          tipo_subyacente = "accion sin rendimiento",tipo_derivado = "call",camino_precio = "D-U")

############### Ejercicio de parcial de Lara #########
#1C 2018 2T
S = 27
K = 2
t = 0.5
n = 2
tlr = 0.27/4
vol = 0.25

valuacion <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = tlr ,t = t,vol = vol,pasos = n,tipo_tasa = "efectiva",tipo_derivado = "call") # tasa tlr porque es futuro

Arbitraje(Precios_subyacente = valuacion$precios_subyacente,prima_mercado = 2,prima_teorica = valuacion$val_derivado[1,1],
          deltas_desc = valuacion$deltas_descontados,
          deltas = valuacion$deltas,tlr = tlr,t = t,n = n,tipo_subyacente = "futuro",tipo_derivado = "call",camino_precio = "U-D")

#1C 2022 1T
S = 20.404
K = 21
t = 0.5
n = 2
tlr = 0.08
tasa = 0.08
vol = 0.20

valuacion <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = tlr ,t = t,vol = vol,pasos = n,tipo_tasa = "continua",tipo_derivado = "call") # tasa tlr porque es futuro

Arbitraje(Precios_subyacente = valuacion$precios_subyacente,prima_mercado = 0.5,prima_teorica = valuacion$val_derivado[1,1],
          deltas_desc = valuacion$deltas_descontados,
          deltas = valuacion$deltas,tlr = tlr,t = t,n = n,tipo_subyacente = "futuro",tipo_derivado = "call",camino_precio = "D-D")


# 2C 2019 1T

S = 50
K = 51
t = 8/12
n = 2
tlr = 0.08
tasa = 0.05
vol = 0.25

valuacion <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = tasa,t = t,vol = vol,pasos = n,tipo_tasa = "continua",tipo_derivado = "call") # tasa tlr porque es futuro

Arbitraje(Precios_subyacente = valuacion$precios_subyacente,prima_mercado = 5,prima_teorica = valuacion$val_derivado[1,1],
          deltas_desc = valuacion$deltas_descontados,
          deltas = valuacion$deltas,tlr = tlr,t = t,n = n,tipo_subyacente = "accion con rendimiento",tipo_derivado = "call",camino_precio = "D-D")

