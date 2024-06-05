####  FORMULAS ###

# Modelo Binomial - Valuar el derivado
valuacion_derivado <- function(S0,K,tlr,tasa,t,vol,pasos,u = NULL, d = NULL,tipo_tasa = "continua",tipo_derivado = "call"){
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

# Modelo Binomial - Arbitraje
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

# Call - Put Parity

Arbitraje_CP_Parity <- function(S,ST,K,c,p,TLR,t, tipo_tasa = "continua",tipo_subyacente = "accion sin dividendo"){
  # TLR y t tienen que estan alineados
  
  filas <- c("Call","Put","Subyacente","Deuda","Inversion","Sumatoria")
  columnas <- c("Cantidad","Flujo_de_fondo")
  
  df <- data.frame(matrix(0,nrow=6,ncol=2,dimnames = list(filas,columnas)))
  df_2 <- data.frame(matrix(0,nrow=6,ncol=2,dimnames = list(filas,columnas)))
  put_parity <- NULL
  call_parity <- NULL
  
  if (tipo_tasa == "continua"){
    FC <- exp(TLR*t)
    FD <- exp(-TLR*t)
  } else if (tipo_tas == "efectiva"){
    FC <- (1+TLR)^t
    FD <- (1+TLR)^-t
  }
  if (tipo_subyacente == "accion sin dividendo") {
    put_parity <- p + ST
    call_parity <- c + K*FD 
  } else if (tipo_subyacente == "futuro"){
    put_parity <- p + S * FD
    call_parity <- c + K*FD
  }
  if (isFALSE(put_parity == call_parity) ) {
    if (call_parity > put_parity) {
      cat("Vendo Call - Vendo Subyacente - Compro Put - Invierto\n")
      dif <- call_parity - put_parity
      
      df["Call","Cantidad"] <- -1
      df["Call","Flujo_de_fondo"] <- c
      df["Put","Cantidad"] <- 1
      df["Put","Flujo_de_fondo"] <- -p
      df["Subyacente","Cantidad"] <- -1
      df["Subyacente","Flujo_de_fondo"] <- S0
      df["Deuda","Cantidad"] <- 1
      df["Deuda","Flujo_de_fondo"] <- 0
      df["Inversion","Cantidad"] <- 1
      df["Inversion","Flujo_de_fondo"] <- -sum(df["Call","Flujo_de_fondo"],df["put","Flujo_de_fondo"],df["Subyacente","Flujo_de_fondo"])
      df["Sumatoria","Flujo_de_fondo"] <- sum(df[,"Flujo_de_fondo"])
      
      df_2["Call","Cantidad"] <- -1
      df_2["Call","Flujo_de_fondo"] <- -max(ST-K,0)
      df_2["Put","Cantidad"] <- 1
      df_2["Put","Flujo_de_fondo"] <- max(K-ST,0)
      df_2["Subyacente","Cantidad"] <- -1
      df_2["Subyacente","Flujo_de_fondo"] <- -ST
      df_2["Deuda","Cantidad"] <- 1
      df_2["Deuda","Flujo_de_fondo"] <-  0
      df_2["Inversion","Cantidad"] <- -df["Inversion","Flujo_de_fondo"] * FC
      df_2["Inversion","Flujo_de_fondo"] <- 1
      df_2["Sumatoria","Flujo_de_fondo"] <- sum(df_2[,"Flujo_de_fondo"])
      
    } else {
      cat("Compro Call - Compro Subyacente - Vendo Put - Tomo Deuda\n")
      dif <- put_parity - call_parity
      
      df["Call","Cantidad"] <- 1
      df["Call","Flujo_de_fondo"] <- -c
      df["Put","Cantidad"] <- -1
      df["Put","Flujo_de_fondo"] <- p
      df["Subyacente","Cantidad"] <- 1
      df["Subyacente","Flujo_de_fondo"] <- -S0
      df["Deuda","Cantidad"] <- 1
      df["Deuda","Flujo_de_fondo"] <- -sum(df["Call","Flujo_de_fondo"],df["Put","Flujo_de_fondo"],df["Subyacente","Flujo_de_fondo"])
      df["Inversion","Cantidad"] <- 0
      df["Inversion","Flujo_de_fondo"] <- 0
      df["Sumatoria","Flujo_de_fondo"] <- sum(df[,"Flujo_de_fondo"])
      
      df_2["Call","Cantidad"] <- -1
      df_2["Call","Flujo_de_fondo"] <- max(ST-K,0)
      df_2["Put","Cantidad"] <- 1
      df_2["Put","Flujo_de_fondo"] <- -max(K-ST,0)
      df_2["Subyacente","Cantidad"] <- -1
      df_2["Subyacente","Flujo_de_fondo"] <- ST
      df_2["Deuda","Cantidad"] <- 1
      df_2["Deuda","Flujo_de_fondo"] <- -df["Deuda","Flujo_de_fondo"] * FC
      df_2["Inversion","Cantidad"] <- 0
      df_2["Inversion","Flujo_de_fondo"] <- 0
      df_2["Sumatoria","Flujo_de_fondo"] <- sum(df_2[,"Flujo_de_fondo"])
      
    }
  } else {return(paste("Se valida la condicion call-put parity"))}
  
  return(list = list(df,df_2,Dif=dif))
  
}

# Ejemplo

S0 <- 33
t <- 3/12
K <- 30
Prima_Call <- 3
Prima_Put <- 5
TLR <- 0.12
ST <- S0 * exp(TLR*t)

Arbitraje_CP_Parity(S = S0,ST = ST,K = K,c = Prima_Call,p = Prima_Put,
                    TLR = TLR,t = t,tipo_tasa = "continua",tipo_subyacente = "accion sin dividendo")


