## Letras griegas ##

rm(list=ls())

#------------- Consolidado de las clases. Script flexible con simulacion de precios ---------------
# CALL
tiempo <- c(0:30) # Dias
r <- 0.05 # Tasa libre de riesgo. Anual
S0 <- 49 # Precio inicial
K <- 50 # Precio de Ejercicio
vto_0 <- 180/365 # Vencimiento de la opcion. Anualizado
sigma <- 0.2
cant_s <- 100000
dt <- vto_0/(length(tiempo)-1) # rebalanceo * 365 = para saber cantidad de dias de rebalanceo
t <- seq(0,vto_0,by = dt)
vto <- vto_0 - t


# Precios Simulados
mu <- 0.08
drift <- (mu - 0.5*sigma^2) * dt # MU no aparece en la ecuacoin de BS. El resultado final no deberia ser afectado por ese valor.
# La PNR no importaba para hacer la estrategia. MU no importa, sirve para simular el precio
volatility <- sigma*sqrt(dt)

S <- rep(NA,length(tiempo))
S[1] <- S0

for (i in 2:length(tiempo)) { #Movimiento geometrico browniano
  S[i] <- S[i-1] * exp(drift + volatility * rnorm(1))
}

# Costo teorico del call
d1_0 <-  (log(S0/K) + (r + 0.5*sigma^2)*vto_0 )/ (sigma * sqrt(vto_0))
d2_0 <- d1_0 - sigma * sqrt(vto_0)
prima <- S0 * pnorm(d1_0) - K*exp(-vto_0*r)*pnorm(d2_0)

costo_teorico <- prima * cant_s

# Replica

d1 <- (log(S/K) + (r + 0.5*sigma^2)*vto) / (sigma*sqrt(vto))
delta <- pnorm(d1)
Posicion <- round(delta,2) * cant_s
Compra_venta <- c(Posicion[1],diff(Posicion))
Costo_s <- Compra_venta * S
Valor_s <- Posicion * S

Deuda <- rep(NA,length(tiempo))
Interes <- rep(NA,length(tiempo))
Deuda[1] <- Costo_s[1]

for (i in 2:length(tiempo)) {
  Interes[i] <- Deuda[i-1]*(r*1/52) #Mala a´roximacion
  Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(tiempo,t,S,vto,d1,delta,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)
view(Tabla)
# Liquidacion Final

Call_Entrega <- ifelse(S[length(tiempo)] > K, K*cant_s,0)
Liquido_deuda <- -Deuda[length(tiempo)]
Flujo_final <- Call_Entrega + Liquido_deuda
VP_Costo <- Flujo_final * exp(-r*vto_0)

# Comparacion
VP_Costo + costo_teorico


############# PUT ###################
rm(list=ls())
#------------- Consolidado de las clases. Script flexible con simulacion de precios ---------------
# PUT

# Parametros
tiempo <- c(0:30) # Dias
r <- 0.05 # Tasa libre de riesgo. Anual
S0 <- 49 # Precio inicial
K <- 50 # Precio de Ejercicio
vto_0 <- 180/365 # Vencimiento de la opcion. Anualizado
sigma <- 0.2
cant_s <- 100000
dt <- vto_0/(length(tiempo)-1) # rebalanceo * 365 = para saber cantidad de dias de rebalanceo
t <- seq(0,vto_0,by = dt)
vto <- vto_0 - t


# Precios Simulados
mu <- 0.08
drift <- (mu - 0.5*sigma^2) * dt # MU no aparece en la ecuacoin de BS. El resultado final no deberia ser afectado por ese valor.
# La PNR no importaba para hacer la estrategia. MU no importa, sirve para simular el precio
volatility <- sigma*sqrt(dt)

S <- rep(NA,length(tiempo))
S[1] <- S0

for (i in 2:length(tiempo)) { #Movimiento geometrico browniano
  S[i] <- S[i-1] * exp(drift + volatility * rnorm(1))
}

# Costo teorico del put
d1_0 <-  (log(S0/K) + (r + 0.5*sigma^2)*vto_0 )/ (sigma * sqrt(vto_0))
d2_0 <- d1_0 - sigma * sqrt(vto_0)
prima <-  K*exp(-vto_0*r)*pnorm(-d2_0) - S0 * pnorm(-d1_0)

costo_teorico <- prima * cant_s

# Replica

d1 <- (log(S/K) + (r + 0.5*sigma^2)*vto) / (sigma*sqrt(vto))
delta <- pnorm(d1)-1 # PUT
Posicion <- round(delta,2) * cant_s
Compra_venta <- c(Posicion[1],diff(Posicion))
Costo_s <- Compra_venta * S
Valor_s <- Posicion * S

Deuda <- rep(NA,length(tiempo))
Interes <- rep(NA,length(tiempo))
Deuda[1] <- Costo_s[1]

for (i in 2:length(tiempo)) {
  Interes[i] <- Deuda[i-1]*(r*1/52) #Mala a´roximacion
  Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(tiempo,t,S,vto,d1,delta,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)
#  view(Tabla)
# Liquidacion Final

Put_Entrega <- ifelse(S[length(tiempo)] < K, K*cant_s,0)
Liquido_deuda <- -Deuda[length(tiempo)]
Flujo_final <- Put_Entrega + Liquido_deuda
VP_Costo <- Flujo_final * exp(-r*vto_0)

# Comparacion
VP_Costo + costo_teorico


########## CLASE  GRIEGAS ##########
#Ejemplo Hull (primero)
rm(list=ls())

library(tidyverse)

r <- 0.05
K <- 50
vto_0 <- 20/52
delta_t <- 1/52 # Semanal
sigma <- 0.2
cant_s <- 100000
S0 <- 49
# Costo teorico del call
d1_0 <-  (log(S0/K) + (r + 0.5*sigma^2)*vto_0 )/ (sigma * sqrt(vto_0))
d2_0 <- d1_0 - sigma * sqrt(vto_0)
prima <- S0 * pnorm(d1_0) - K*exp(-vto_0*r)*pnorm(d2_0)

costo_teorico <- prima * cant_s

# Replica

semana <- c(0:20)
t <- seq(0,vto_0,by = 1/52)
precios <- c(49, 48.12, 47.37, 50.25, 51.75, 53.12, 53, 51.87, 51.38, 53, 49.88,
             48.5, 49.88, 50.37, 52.13, 51.88, 52.87, 54.87, 54.62, 55.87, 57.25)
vto <- vto_0 - t

d1 <- (log(precios/K) + (r + 0.5*sigma^2)*vto) / (sigma*sqrt(vto))
delta <- pnorm(d1)
Posicion <- round(delta,3) * cant_s
Compra_venta <- c(Posicion[1],diff(Posicion))
Costo_s <- Compra_venta * precios
Valor_s <- Posicion * precios

Deuda <- rep(NA,21)
Interes <- rep(NA,21)
Deuda[1] <- Costo_s[1]

for (i in 2:21) {
    Interes[i] <- Deuda[i-1]*(r*1/52) #Mala a´roximacion
    Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(semana,t,precios,vto,d1,delta,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)
view(Tabla)
# Liquidacion Final

Call_Entrega <- ifelse(precios[21] > K, K*cant_s,0)
Liquido_deuda <- -Deuda[21]
Flujo_final <- Call_Entrega + Liquido_deuda
VP_Costo <- Flujo_final * exp(-r*vto_0)

# Comparacion
VP_Costo + costo_teorico

########## Ejemplo con simulacion ----------------------------------------------

rm(list=ls())

semana <- c(0:20)
r <- 0.05
K <- 50
vto_0 <- 20/52
delta_t <- 1/52 # Semanal

sigma <- 0.2
cant_s <- 100000
S0 <- 49

dt <- 1/52
t <- seq(0,vto_0,by = dt)

vto <- vto_0 - t


# Precios Simulados
drift <- (mu - 0.5*sigma^2) * dt # MU no aparece en la ecuacoin de BS. El resultado final no deberia ser afectado por ese valor.
                                 # La PNR no importaba para hacer la estrategia. MU no importa, sirve para simular el precio
volatility <- sigma*sqrt(dt)
mu <- 0.08
S <- rep(NA,21)
S[1] <- S0

for (i in 2:length(semana)) { #Movimiento geometrico browniano
  S[i] <- S[i-1] * exp(drift + volatility * rnorm(1))
}

# Costo teorico del call
d1_0 <-  (log(S0/K) + (r + 0.5*sigma^2)*vto_0 )/ (sigma * sqrt(vto_0))
d2_0 <- d1_0 - sigma * sqrt(vto_0)
prima <- S0 * pnorm(d1_0) - K*exp(-vto_0*r)*pnorm(d2_0)

costo_teorico <- prima * cant_s

# Replica

d1 <- (log(S/K) + (r + 0.5*sigma^2)*vto) / (sigma*sqrt(vto))
delta <- pnorm(d1)
Posicion <- round(delta,2) * cant_s
Compra_venta <- c(Posicion[1],diff(Posicion))
Costo_s <- Compra_venta * S
Valor_s <- Posicion * S

Deuda <- rep(NA,21)
Interes <- rep(NA,21)
Deuda[1] <- Costo_s[1]

for (i in 2:21) {
  Interes[i] <- Deuda[i-1]*(r*1/52) #Mala a´roximacion
  Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
}

Tabla <- tibble(semana,t,S,vto,d1,delta,
                Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)

# Liquidacion Final

Call_Entrega <- ifelse(S[21] > K, K*cant_s,0)
Liquido_deuda <- -Deuda[21]
Flujo_final <- Call_Entrega + Liquido_deuda
VP_Costo <- Flujo_final * exp(-r*vto_0)

# Comparacion
VP_Costo + costo_teorico


########## Ejemplo con simulacion guardando valor ----------------------------------------------
rm(list=ls())

# Parametros
r <- 0.05
K <- 50
vto_0 <- 20/52
mu <- 0.08
sigma <- 0.2
cant_s <- 100000
S0 <- 49

semana <- seq(0,20)
dt <- 1/52 / 2
t <- seq(0,vto_0,by = dt)
vto <- vto_0 - t

# Estrategia neutral simulada N veces
n <- 10000
Call_Entrega <- rep(NA,n)
Liquido_deuda <- rep(NA,n)
Flujo_final <- rep(NA,n)
VP_Costo <- rep(NA,n)

#Precios Simulados
drift <- (mu - 0.5*sigma^2) * dt
volatility <- sigma*sqrt(dt)

S <- rep(NA,length(t))
S[1] <- S0

for (j in 1:n) {
  S[1] <- S0
  # Simulacion Precios
  
  for (i in 2:length(t)) { #Movimiento geometrico browniano
    S[i] <- S[i-1] * exp(drift + volatility * rnorm(1))
  }
    
    # Replica
    
    d1 <- (log(S/K) + (r + 0.5*sigma^2)*vto) / (sigma*sqrt(vto))
    delta <- pnorm(d1)
    Posicion <- round(delta,2) * cant_s
    Compra_venta <- c(Posicion[1],diff(Posicion))
    Costo_s <- Compra_venta * S
    Valor_s <- Posicion * S
    
    Deuda <- rep(NA,21)
    Interes <- rep(NA,21)
    Deuda[1] <- Costo_s[1]
    
    for (i in 2:length(t)) {
      Interes[i] <- Deuda[i-1]*(r*1/52) #Mala a´roximacion
      Deuda[i] <- Deuda[i-1] + Interes[i] + Costo_s[i]
    }
    
#    Tabla <- tibble(semana,t,S,vto,d1,delta,
#                    Posicion,Compra_venta,Costo_s,Deuda,Interes,Valor_s)
    
    # Liquidacion Final
    
    Call_Entrega[j] <- ifelse(S[length(t)] > K, K*cant_s,0)
    Liquido_deuda[j] <- -Deuda[length(t)]
    Flujo_final[j] <- Call_Entrega[j] + Liquido_deuda[j]
    VP_Costo[j] <- Flujo_final[j] * exp(-r*vto_0)
}

# Costo teorico del call
d1_0 <-  (log(S0/K) + (r + 0.5*sigma^2)*vto_0 )/ (sigma * sqrt(vto_0))
d2_0 <- d1_0 - sigma * sqrt(vto_0)
prima <- S0 * pnorm(d1_0) - K*exp(-vto_0*r)*pnorm(d2_0)

costo_teorico <- prima * cant_s


# Analisis Resultados
Prima_cobrada <- costo_teorico # Cambio la prima teorica cobrada asi gano mas. Cambia la probabilidad pero no el desempeño.
Resultado <- VP_Costo + Prima_cobrada

Promedio <- mean(Resultado)
Desvio <- sd(Resultado)

Desempeño <- sd(Resultado)/costo_teorico

sum(Resultado < 0)/n # Proximado de la probabilidad

# ¿ Que puedo hacer para aperder un poco menos o achicar las chances de perder. O hacer que las perdidas sean menos grandes ?
# Disminuir la frecuencia de rebalanceo
