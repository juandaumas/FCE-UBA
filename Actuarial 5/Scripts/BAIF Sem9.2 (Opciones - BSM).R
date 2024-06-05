# BSM - Clase 03-06-23

# La ecuacion de BSM NO DEPENDE DEL RENDIMIENTO DEL ACTIVO.
# NO es un supuesto que los agentes son neutrales al riesgo. No hay portunidades de arbitraje por eso el actibo sigue un
# rendimiento similar al riesgo nuestro.

# Formulas cerradas de la BSM
rm(list = ls())

S <- 100   # Precio subyacente
K <- 90   # Precio de ejercicio
r <- 0.05  # Tasa de interés libre de riesgo
t <- 1     # Tiempo hasta la expiración (en años)
sigma <- 0.2  # Volatilidad

Valuacion_BSM <- function(S, K, r, t, sigma, type) {
  
  d1 <- (log(S / K) + (r + sigma^2 / 2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  if (type == "call") {
    value <- S * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  } else if (type == "put") {
    value <- K * exp(-r * t) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("El tipo de opcion tiene que ser call o put")
  }
  return(value)
}

Valuacion_BSM(S,K,r,t,sigma,"call")
Valuacion_BSM(S,K,r,t,sigma,"put")

ST <- S * exp((r-0.5*sigma^2)*t + sigma * rnorm(10000)*sqrt(t)) #Mundo riesgo neutro
ST

Call_t <- sapply(ST, function(x) max(x-K,0))
c_media_sim <- mean(Call_t) * exp(-r * t)

Put_t <- sapply(ST, function(x) max(K-x,0))
p_media_sim <- mean(Put_t) * exp(-tlr * vto)
