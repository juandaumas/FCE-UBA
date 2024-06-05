### Parcial 2019C2 Profe: Lara Vazquez.

## FORMULAS ##
valuacion_derivado <- function(S0,K,tlr,tasa,t,vol,pasos,tipo_tasa = "continua",tipo_derivado = "put"){
  
  n <- pasos
  
  u <- exp(vol*sqrt(t/n))
  d <- 1/u
  
  if (tipo_tasa == "continua"){
    P <- (exp((tlr-tasa)*t/n)-d)/(u-d)
    FD <- exp(-(tlr*t/n))
  } else if(tipo_tasa == "efectiva"){
    P <- (((1+tlr)/(1+tasa))-d)/(u-d)
    FD <- ((1+tlr)^-1)
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
  
  return(list(P=P,FD=FD,U=u,D=d,precios_subyacente = subyacente_precios, val_derivado = val_derivado))
}

##
#   Se negocia en el mercado un put con precio de ejercicio 51 sobre una acción que paga dividendos a una tasa 5% (continua)
#   y que tiene vencimiento en 8 meses. La acción hoy se negocia a $50 y muestra una volatilidad histórica anual de 25%. La
#   tasa libre de riesgo de la economía en donde cotiza la acción es del 8% (tasa continua).
#   a. Construir un modelo binomial de dos pasos para determinar el precio del put
#   b. Identificar si existen oportunidades de arbitraje – especificando cuáles – si el put se negocia en el mercado a $ 3.2
#   c. Verificar cómo funciona la estrategia planteada en (b) si el precio de la acción sube en el primer periodo y baja en el
#   segundo.
#   d. Valuar un call sobre el mismo subyacente – y con mismo strike y vencimiento – a partir de la Call Put Parity.
#   Demuestre esta paridad.

S <- 50
K <- 51
Q <- 0.05 #Continua
t <- 8/12
n <- 2 #Pasos
vol <- 0.25
tlr <- 0.08 #Continua

Derivado <- valuacion_derivado(S0 = S,K = K,tlr = tlr,tasa = Q,t = t,vol =  vol,pasos = 2,tipo_tasa = "continua",tipo_derivado = "put")

# replica

deltas <- matrix(NA,ncol=2,nrow=2)

deltas[1,2] <- (Derivado$val_derivado[1,3]-Derivado$val_derivado[2,3])/(Derivado$precios_subyacente[1,3]-Derivado$precios_subyacente[2,3])
deltas[2,2] <- (Derivado$val_derivado[2,3]-Derivado$val_derivado[3,3])/(Derivado$precios_subyacente[2,3]-Derivado$precios_subyacente[3,3])
deltas[1,1] <- (Derivado$val_derivado[1,2]-Derivado$val_derivado[2,2])/(Derivado$precios_subyacente[1,2]-Derivado$precios_subyacente[2,2])

deltas_descontados <- deltas*exp(-Q*t/n)

# 2 - Put teorico 3.7 
#     Put Mercado 3.2

# Put Mercado < Put Teorico - Vendo el teorico. Compro el de mercado.
# C + K*e^-rt = P + S

# Compro put
# Vendo Suby
# Invierto
