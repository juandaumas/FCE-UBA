# 900496 - Juan Daumas - Segundo Parcial BAIF

rm(list=ls())
graphics.off()

# Librerias
library(tidyverse)

# Parametros
Acciones <- c("AAPL","IBM","GOOG")
Cantidad <- c(250,300,300)
Precio <- c(175,135,125)
Volatilidad_anual <- c(0.23,0.21,0.20)
Tiempo <- 10/252
alpha <- 0.01

# Correlaciones
rhos <- matrix(c(1, 0.25, 0.10,
                 0.25, 1, 0.9,
                 0.10, 0.9, 1), nrow = 3)

# Matriz de Varianza y Covarianza
matriz_cov <- diag(Volatilidad_anual) %*% rhos %*% diag(Volatilidad_anual)


# Calculos para el VAR
zc <- qnorm(1 - alpha) # Z Critico
h <- sqrt(Tiempo) # Tiempo
W <- Cantidad * Precio
WT <- sum(W)
Ponderaciones_activos <- W/WT

# A ---------------------------------------------------------------------------------------------
# Var Individual
Var_individual <- W * Volatilidad_anual * zc * h 
Var_individual_df <- data.frame(Acciones = Acciones, Var_individual = Var_individual)

Var_individual_df
# B ----------------------------------------------------------------------------------------------

# Calcular la varianza del portafolio
# Varianza del portafolio = (w1^2 * Varianza1) + (w2^2 * Varianza2) + (w3^2 * Varianza3) + 2 * (w1 * w2 * Covarianza1_2 + w1 * w3 * Covarianza1_3 + w2 * w3 * Covarianza2_3)
varianza_portafolio <- t(matrix(Ponderaciones_activos)) %*% matriz_cov %*% matrix(Ponderaciones_activos)
volatilidad_portafolio <- sqrt(varianza_portafolio)

# Calculo VAR diversificado
VaR <- WT*h*zc*volatilidad_portafolio

cat("El var diversificado es:",VaR," El beneficio de diversificar es:",sum(Var_individual)-VaR,
    ". Se redujo el riesgo de la cartera en un ",as.double((sum(Var_individual)-VaR)/sum(Var_individual))*100,"%")

# C ----------------------------------------------------------------------------------------------
# Component Var
betas <- t(Ponderaciones_activos %*% matriz_cov/as.double(varianza_portafolio))
component_var <- t(VaR %*% t(betas * Ponderaciones_activos))

component_var_df <- data.frame(Acciones,component_var)
component_var_df
cat("El component var permite entender cuánto aporta cada activo al VaR. Indica cuánto cambiaría el VaR de la cartera si se elimina el activo.")
# D ----------------------------------------------------------------------------------------------
cat("FALSO. La accion que mas riesgo genera en la cartera es IBM. Esto se debe a su correlacion con los demas activos. Se puede observar como influye en el VaR a traves del Component Var, el cual es el más alto")

