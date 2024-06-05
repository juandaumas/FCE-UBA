## Volatilidad Implicita ##
rm(list=ls())

library(ggplot2)
library(tidyverse)

# Función de Black-Scholes para el cálculo del precio de la opción
black_scholes <- function(S, K, r, t, sigma, tipo) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  if (tipo == "call") {
    opcion_precio_teorico <- S * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  } else if (tipo == "put") {
    opcion_precio_teorico <- K * exp(-r * t) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Tipo tiene que ser 'call' or 'put'.")
  }
  
  return(opcion_precio_teorico)
}

# Derivada parcial de Black-Scholes con respecto a la volatilidad (vega)
vega <- function(S, K, r, t, sigma,Q) {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * t) / (sigma * sqrt(t))
  vega <- S * dnorm(d1) * sqrt(t) * exp(-Q*t)
  
  return(vega)
}

# Método de Newton-Raphson para encontrar la volatilidad implícita
newton_raphson_volatility <- function(opcion_precio_mercado, S, K, r, t, tipo, tol, max_iter,Q,valor_inicial) {
  sigma <- valor_inicial # Valor inicial de la volatilidad
  iter <- 0
  
  while (iter < max_iter) {
    price_diff <- black_scholes(S, K, r, t, sigma, tipo) - opcion_precio_mercado
    vega_value <- vega(S, K, r, t, sigma,Q)
    
    if (abs(price_diff) < tol) {
      break
    }
    
    sigma <- sigma - price_diff / vega_value
    iter <- iter + 1
  }
  
  if (iter == max_iter) {
    return(NULL)  # No se alcanzó convergencia
  } else {
    return(sigma)  # Se encontró una volatilidad implícita aproximada
  }
}

# Parámetros
S <- 120 # Precio subyacente
K <- 120  # Precio de ejercicio
r <- 0.05 # Tasa libre de riesgo
Q <- 0.03 # Tiempo de vencimiento (en años)
t <- 0.75
opcion_precio_mercado <- 9.121237 # Precio observado de la opción en el mercado
tipo <- "call"   # Tipo de opción: "call" o "put"
tol <- 0.000001  # Tolerancia para la convergencia
max_iter <- 10000  # Máximo número de iteraciones


# Calcular la volatilidad implícita utilizando el método de Newton-Raphson
volatility <- newton_raphson_volatility(opcion_precio_mercado, S, K, r, t, tipo, tol, max_iter,Q,0.1)

# Resultado
if (is.null(volatility)) {
  cat("No se encontró una volatilidad implícita en las", max_iter, "iteraciones.")
} else {
  cat("La volatilidad implícita aproximada es:", volatility)
}

# Parámetros
S <- 119 # Precio subyacente
K <- 117  # Precio de ejercicio
r <- 0.5/100 # Tasa libre de riesgo
Q <- 0.03 # Tiempo de vencimiento (en años)
t <- 12/365
opcion_precio_teorico <- 3 # Precio observado de la opción en el mercado
tipo <- "call"   # Tipo de opción: "call" o "put"
tol <- 0.000001  # Tolerancia para la convergencia
max_iter <- 10000  # Máximo número de iteraciones


# Calcular la volatilidad implícita utilizando el método de Newton-Raphson
volatility <- newton_raphson_volatility(opcion_precio_teorico, S, K, r, t, tipo, tol, max_iter,Q,0.1)

# Resultado
if (is.null(volatility)) {
  cat("No se encontró una volatilidad implícita en las", max_iter, "iteraciones.")
} else {
  cat("La volatilidad implícita aproximada es:", volatility)
}

