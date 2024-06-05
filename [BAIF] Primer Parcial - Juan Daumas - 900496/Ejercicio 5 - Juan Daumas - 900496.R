# ---- Ejercicio 5 - Primer Parcial - Juan Daumas - 900496

rm(list=ls())
graphics.off()
library(tidyverse)
library(ggplot2)

# ---- Ejercicio 5)a -----

S <- 120   # Precio subyacente
K <- 115   # Precio de ejercicio
mu <- 0.18
sigma <- 0.25  # Volatilidad
r <- 0.06  # Tasa de interés libre de riesgo
t <- 2/12     # Tiempo hasta la expiración (en años)
cant_s <- 50000

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
paste("La prima del contrato es:",costo_teorico)



# ---- Ejercicio 5)b ----
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



# ---- Ejercicio 5)c)----

semana_t1 <- c(0,1)
dt_t1 <- 1/48
vencimiento_t1 <- c(t,t-dt_t1)
precios_t1 <- c(120,122)
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
  Interes_t1[i] <- Deuda_t1[i-1]*(r*dt_t1)
  Deuda_t1[i] <- Deuda_t1[i-1] + Interes_t1[i] + Costo_s_t1[i]
}

Tabla_t1 <- tibble(semana_t1,vencimiento_t1,precios_t1,d1_t1,deltas_t1,
                   Posicion_t1,Compra_venta_t1,Costo_s_t1,Deuda_t1,Interes_t1,Valor_s_t1)
view(Tabla_t1)

paste("Una vez armada la cartera al momento 0, si el precio en la primer semana se modifica a",precios_t1[2],
      "tengo que tener",round(deltas_t1[2]*1000,0)," subyacentes. Por lo tanto vendo",round(-Compra_venta_t1[2],0),"de subyacente.",
      "Mi deuda acumulada hasta el momento es:",Deuda_t1[2])


# ---- Ejercicio 5)d) ----

semanas_estrategia <- seq(0,8)
n_long <- length(semanas_estrategia)
dt <- 1/48
vencimiento <- seq(t,0,by = -dt)
precios <- c(120,122,118,114,111,114,110,108,113)
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
  Interes[i] <- Deuda[i-1]*(r*dt) 
  Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(semanas_estrategia,vencimiento,precios,d1,deltas,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)

# Liquidacion Final

Call_Entrega <- ifelse(precios[n_long] > K, K*cant_s,0) 
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


#---- Ejercicio 5)e) ----

cant_sim = 10000

Call_Entrega <- rep(NA,cant_sim)
Liquido_deuda <- rep(NA,cant_sim)
Flujo_final <- rep(NA,cant_sim)
VP_Costo <- rep(NA,cant_sim)

# Precios Simulados
drift <- (mu - 0.5*sigma^2) * dt
volatility <- sigma*sqrt(dt)

precio_inicial <- 120
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

alpha <- 0.01/2
IC <- c(alpha,1-alpha)
IC <- IC*cant_sim

valor_min <- sort(Resultado_sim)[IC][1]
valor_max <- sort(Resultado_sim)[IC][2]

cat("Intervalo de confianza del 99% entre: (", valor_min, ", ", valor_max, ")")


