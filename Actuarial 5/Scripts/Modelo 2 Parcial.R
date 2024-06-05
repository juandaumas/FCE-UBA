## Modelo de Parcial

rm(list=ls())
graphics.off()

# Librerias
library(ggplot2)
library(tidyverse)

# Ejercicio 1 A ----------------------------------------------------------------

S <- 75   # Precio subyacente
K <- 78   # Precio de ejercicio
mu <- 0.20
sigma <- 0.3  # Volatilidad
r <- 0.08  # Tasa de interés libre de riesgo
t <- 2/12     # Tiempo hasta la expiración (en años)
cant_s <- 1000

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

prima_teorica_bsm <- black_scholes(S,K,r,t,sigma,"call")
costo_teorico <- prima_teorica_bsm * cant_s

paste("La prima de la opcion valuada por BSM es:",prima_teorica_bsm)
paste("Su costo teorico es:",costo_teorico)

# Ejercicio 1 B ----------------------------------------------------------------
cant_sim <- 10000
ST_pnr <- S * exp((r-0.5*sigma^2)*t + sigma * rnorm(cant_sim)*sqrt(t)) # Mundo riesgo neutro

mean(ST_pnr) ; S*exp(r*t) # Verificacion simple

Call_t <- sapply(ST_pnr, function(x) max(x-K,0))

prima_teorica_simulacion <- mean(Call_t) * exp(-r * t)
prima_teorica_simulacion

diferencia <- prima_teorica_bsm - prima_teorica_simulacion #"En la teoria la diferencia deberia ser muy cercana a 0"

paste("La diferencia entre la prima teorica a traves de BSM y una simulacion del precio del subaycente en un mundo neutral al riesgo es de:",diferencia)

# Ejercicio 1 C ----------------------------------------------------------------

type = "call"
d1 <- (log(S / K) + (r + sigma^2 / 2) * t) / (sigma * sqrt(t))
delta <- pnorm(d1) # El delta es positivo por lo tanto tengo que comprar subyacente
cant_sub <- round(delta * cant_s,0)

if (type=="call") {
  if (delta>0) {
    paste("Dado que el vendi el call tengo que cubrirme por ",cant_sub," de subyacente para generar una cartera delta neutral")
  } else if (delta<0) { paste("Dado que compre el call tengo que cubrirme vendiendo ",cant_sub," de subyacente para generar una cartera delta neutral") }
} else if (type == "put") {
  if (delta > 0) {
    paste("Dado que compre el put tengo que comprar ",cant_sub," de subyacente para generar una cartera delta neutral") 
  }
} else if (delta < 0){
  paste("Dado que vendi el put tengo que vender ",cant_sub," de subyacente para generar una cartera delta neutral") 
}

paste("Por lo tanto desembolso",S*cant_sub," para comprar los",cant_sub," de activos. Me endeudo a una tasa del",r*100,"% continua anual.")


# Ejercicio 1 D ----------------------------------------------------------------

  semana_t1 <- c(0,1)
  dt_t1 <- 1/48
  vencimiento_t1 <- c(t,t-dt_t1)
  precios_t1 <- c(75,72)
  d1_t1 <- (log(precios_t1/K) + (r + 0.5*sigma^2)*vencimiento_t1) / (sigma*sqrt(vencimiento_t1))
  deltas_t1 <- pnorm(d1_t1)
  Posicion_t1 <- deltas_t1 * cant_s
  Compra_venta_t1 <- c(Posicion_t1[1],diff(Posicion_t1))
  Costo_s_t1 <- Compra_venta_t1 * precios_t1
  Valor_s_t1 <- Posicion_t1 * precios_t1
  
  Deuda_t1 <- rep(NA,length(semana_t1))
  Interes_t1 <- rep(NA,length(semana_t1))
  Deuda_t1[1] <- Costo_s_t1[1]

for (i in 2:2) {
  Interes_t1[i] <- Deuda_t1[i-1]*(r*dt_t1) # Mala aproximacion
  Deuda_t1[i] <- Deuda_t1[i-1] + Interes_t1[i] + Costo_s_t1[i]
}

Tabla_t1 <- tibble(semana_t1,vencimiento_t1,precios_t1,d1_t1,deltas_t1,
                Posicion_t1,Compra_venta_t1,Costo_s_t1,Deuda_t1,Interes_t1,Valor_s_t1)
# view(Tabla_t1)

paste("Una vez armada la cartera al momento 0, si el precio en la primer semana se modifica a",precios_t1[2],
      "tengo que tener",deltas_t1[2]*1000," subyacentes. Por lo tanto vendo",-Compra_venta_t1[2],"de subyacente.",
      "Mi deuda acumulada hasta el momento es:",Deuda_t1[2])

# Ejercicio 1 E ----------------------------------------------------------------
semanas_estrategia <- seq(0,8)
n_long <- length(semanas_estrategia)
dt <- 1/48
vencimiento <- seq(t,0,by = -dt)
precios <- c(75,72,70,72,74,75,77,79,80)
d1 <- (log(precios/K) + (r + 0.5*sigma^2)*vencimiento) / (sigma*sqrt(vencimiento))
deltas <- pnorm(d1)
Posicion <- deltas * cant_s
Compra_venta <- c(Posicion[1],diff(Posicion))
Costo_s <- Compra_venta * precios
Valor_s <- Posicion * precios

Deuda <- rep(NA,n_long)
Interes <- rep(NA,n_long)
Deuda[1] <- Costo_s[1]

for (i in 2:n_long) {
  Interes[i] <- Deuda[i-1]*(r*dt) #Mala aproximacion
  Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(semanas_estrategia,vencimiento,precios,d1,deltas,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)

# Liquidacion Final

Call_Entrega <- ifelse(precios[n_long] > K, K*cant_s,0) # Vendo los activos
Liquido_deuda <- -Deuda[n_long]
Flujo_final <- Call_Entrega + Liquido_deuda
VP_Costo <- Flujo_final * exp(-r*t)

# Comparacion
costo_teorico <- prima_teorica_bsm * cant_s
Resultado <- VP_Costo + costo_teorico

# Resultado + Analisis
cat("Por haber vendido el call me ejercen y tengo que vender",cant_sub,
    "subyacentes a un precio de",K,". Recibo:",Call_Entrega," y pago",Liquido_deuda," de deuda. 
    La diferencia entre lo recibido por lanzar calls (recibir la prima) y el costo de la estrategia es:",Resultado)

# Ejercicio 1 F ----------------------------------------------------------------

Call_Entrega <- rep(NA,cant_sim)
Liquido_deuda <- rep(NA,cant_sim)
Flujo_final <- rep(NA,cant_sim)
VP_Costo <- rep(NA,cant_sim)

# Precios Simulados
drift <- (mu - 0.5*sigma^2) * dt
volatility <- sigma*sqrt(dt)

precio_inicial <- 75
S_simulacion <- rep(NA,length(vencimiento))
S_simulacion[1] <- precio_inicial

for (j in 1:cant_sim) {
  
  # Simulacion Precios
  for (i in 2:length(vencimiento)) { #Movimiento geometrico browniano
    S_simulacion[i] <- S_simulacion[i-1] * exp(drift + volatility * rnorm(1))
  } # Precios reales de subyacentes con rentabilidad del activo. No depende del MU el desempeño de la estrategia.
  
  # Replica
  d1 <- (log(S_simulacion/K) + (r + 0.5*sigma^2)*vencimiento) / (sigma*sqrt(vencimiento))
  delta <- pnorm(d1)
  Posicion <- round(delta * cant_s,0)
  Compra_venta <- c(Posicion[1],diff(Posicion))
  Costo_s <- Compra_venta * S_simulacion
  Valor_s <- Posicion * S_simulacion
  
  Deuda <- rep(NA,n_long)
  Interes <- rep(NA,n_long)
  Deuda[1] <- Costo_s[1]
  
  for (i in 2:n_long) {
    Interes[i] <- Deuda[i-1]*(r*dt) #Mala aproximacion
    Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
  }
  
  # Liquidacion Final
  Call_Entrega[j] <- ifelse(S_simulacion[n_long] > K, K*cant_s,0)
  Liquido_deuda[j] <- -Deuda[n_long]
  Flujo_final[j] <- Call_Entrega[j] + Liquido_deuda[j]
  VP_Costo[j] <- Flujo_final[j] * exp(-r*t)
}

Resultado_sim <- VP_Costo + costo_teorico

media <- mean(Resultado_sim)
desviacion <- sd(Resultado_sim)
Desempeño <- sd(Resultado_sim)/costo_teorico

alpha <- 0.05/2
IC <- c(alpha,1-alpha)
IC <- IC*cant_sim

valor_min <- sort(Resultado_sim)[IC][1]
valor_max <- sort(Resultado_sim)[IC][2]

paste("Intervalo de confianza del 95% entre: (", valor_min, ", ", valor_max, ")")

# Ejercicio 1 G ----------------------------------------------------------------

Prima_cobrada <- 4150 # Cambio la prima teorica cobrada asi gano mas. Cambia la probabilidad pero no el desempeño.
Resultado_sim <- VP_Costo + Prima_cobrada

# hist(Resultado_sim)
sum(Resultado_sim < 0)/cant_sim # Proximado de la probabilidad

paste("La prima deberia ser aproximadamente",Prima_cobrada/1000)

# Ejercicio 1 H ----------------------------------------------------------------

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
S <- 75
K <- 78
opcion_precio_mercado <- 4.1 # Precio observado de la opción en el mercado
tipo <- "call"   # Tipo de opción: "call" o "put"
tol <- 0.000001  # Tolerancia para la convergencia
max_iter <- 10000  # Máximo número de iteraciones
Q <- 0 # El subyacente no tiene rendimientos asociados.

# Calcular la volatilidad implícita utilizando el método de Newton-Raphson
volatility <- newton_raphson_volatility(opcion_precio_mercado, S, K, r, t, tipo, tol, max_iter,Q,0.3) # Valor de sigma = 0.3

# Resultado
if (is.null(volatility)) {
  cat("No se encontró una volatilidad implícita en las", max_iter, "iteraciones.")
} else {
  cat("La volatilidad implícita aproximada es:", volatility)
}

# Modificar las K para sacar la volatility smile. Primas y K 

# Sacar volatility smile y hacer grafico -- Revisar

S <- 75
strikes <- c(78,80,79)
opcion_precio_mercado <- c(9,4.1,4.5,3) # Precio observado de la opción en el mercado
tipo <- "call"   # Tipo de opción: "call" o "put"
tol <- 0.000001  # Tolerancia para la convergencia
max_iter <- 1000  # Máximo número de iteraciones
Q <- 0 # El subyacente no tiene rendimientos asociados.

volatilidad_implicita <- rep(NA,length(strikes))
for (i in 1:length(strikes)) {
  volatilidad_implicita[i] <- newton_raphson_volatility(opcion_precio_mercado[i], S, strikes[i], r, t, tipo, tol, max_iter,Q,valor_inicial = 0) # Valor de sigma = 0.3
}
volatilidad_implicita <- newton_raphson_volatility(opcion_precio_mercado[1], S, strikes[1], r, t, tipo, tol, max_iter,Q,valor_inicial = 0.3) # Valor de sigma = 0.3


# Crear un dataframe con los datos
data_plot <- data.frame(strikes,volatilidades)

# Graficar los valores de volatilidad
ggplot(data = data_plot, aes(x = strikes, y = volatilidades)) +
  geom_line() +
  xlab("Precio de Strike") +
  ylab("Volatilidad Implícita")

#### Backtesting ###

