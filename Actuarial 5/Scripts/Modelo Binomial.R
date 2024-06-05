## Modelo Binomial
rm(list=ls())
## Definir parametros para todos igual

tlr <- 0.26/4 # TNA cap trim
vol <- 0.2
S0 <- 27
K <- 28
t <- 0.5
n <- 2 # cantidad de pasos
tasa <- 0.26/4

u <- exp(vol*sqrt(t/n))
d <- 1/u


mod_bin <- function(volatilidad_anual,cantidad_pasos,tiempo, tipo_tasa = "Continua"){
  
  u <- exp(volatilidad_anual*sqrt(tiempo/cantidad_pasos))
  d <- 1/u
  
  # Continua. Si es futuro entonces exp(tasa-r) = 1
  if (tipo_tasa == "Continua"){
    P <- (exp(tasa-tlr)-d)/(u-d)
  } else {
    P <- (((1+tasa)/(1+tlr))-d)/(u-d)  
  }
  # Crear un data frame con una fila y tres columnas
  mtx <- matrix(data = c(u, d, P), nrow = 1, ncol = 3, dimnames = list("1", c("U", "D", "P")))
  df <- as.data.frame(mtx)
  return(df)
}
  
datos <- mod_bin(vol,n,t,tipo_tasa = "efectiva")

u <- exp(vol * sqrt(t/n))
d <- 1 / u
P <- (exp(tasa-tlr)-d)/(u-d)

precios <- matrix(0, n + 1, n + 1)
precios[1, 1] <- S0

for (i in 1:(n+1)) {
  for (j in 2:(n+1)) {
    precios[i, j] <- precios[i, j-1] * u
    precios[j, j] <- precios[j-1, j-1] * d
  }
}
precios

derivado = "Put"
val_derivado <- matrix (0, n + 1, n + 1)
{
  for (i in (n+1):1){
    if (derivado == "Put") {val_derivado[i,n+1] <- max(0,K - precios[i,n+1])} else if(derivado == "Call") {val_derivado[i,n+1] <- max(0, precios[i,n+1] - K)}
  }
}

FD <- ((1+tlr)^-1) # es efectiva

for (i in n:1){
  for (j in i:1){
    val_derivado[j,i] <- (P * val_derivado[j,i+1] + (1-P) * val_derivado[j+1,i+1] ) * FD
  }
}



#####################################################

# Función para valuar el activo y su trayectoria de precios
calcular_precios <- function(S0, tlr,tasa, vol, n, t, tipo_tasa = "continua") {
  delta_t <- t / n
  u <- exp(vol * sqrt(delta_t))
  d <- 1 / u
  if (tipo_tasa == "continua") {p <- (exp((tasa-tlr) * delta_t) - d) / (u - d)} 
  else {p <- ((((1+tasa)/(1+tlr))) - d) / (u - d)}
  
  precios <- matrix(0, n + 1, n + 1)
  precios[1, 1] <- S0
  for (i in 2:(n+1)) {
    precios[i, 1] <- precios[i-1, 1] * u
    for (j in 2:(i+1)) {
      precios[i, j] <- precios[i-1, j-1] * d
    }
  }
  return(precios)
}

# Función para obtener la PNR
calcular_PNR <- function(precios, K, n) {
  pnr <- matrix(0, n+1, n+1)
  for (i in 1:(n+1)) {
    for (j in 1:(i+1)) {
      pnr[i,j] <- max(0, precios[i,j] - K)
    }
  }
  return(pnr)
}

# Función para valuar el derivado
calcular_valor_derivado <- function(pnr, tasa, n, t) {
  delta_t <- t / n
  u <- exp(tasa * delta_t)
  d <- 1 / u
  p <- (exp(tasa * delta_t) - d) / (u - d)
  valor_derivado <- matrix(0, n+1, n+1)
  valor_derivado[, n+1] <- pnr[, n+1]
  for (i in n:1) {
    for (j in 1:i) {
      valor_derivado[j, i] <- exp(-tasa * delta_t) * (p * valor_derivado[j, i+1] + (1-p) * valor_derivado[j+1, i+1])
    }
  }
  return(valor_derivado)
}

# Parámetros
tlr <- 0.26/4 # TNA cap trim
vol <- 0.2
S0 <- 27
K <- 28
t <- 0.5
n <- 2 # cantidad de pasos
tasa <- 0.26/4

# Calculamos los precios
precios <- calcular_precios(S0 = S0,tlr = tlr,tasa = tasa,vol = vol,n = n,t = t,tipo_tasa = "continua")

# Calculamos la PNR
pnr <- calcular_PNR(precios, K, n)

# Calculamos el valor del derivado
valor_derivado <- calcular_valor_derivado(pnr, tasa, n, t)

# Imprimimos el valor del derivado
valor_derivado[1,1]

