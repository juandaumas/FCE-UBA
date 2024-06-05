## BAIF Sem12 (Riesgos - Mercado)
rm(list=ls())
library(tidyverse)
library(ggplot2)
# Ejemplos
  # Parametros
  cant_suby_a <- 2100
  cant_suby_b <- 5000
  precio_suby_a <- 5.46
  precio_suby_b <- 3.25
  rendimiento_anual_a <- 0.15
  rendimiento_anual_b <- 0.17
  varianza_a <- 0.1
  varianza_b <- 0.11
  covarianza_ab <- 0.052
  
  tiempo <- 1/252
  
  # Niveles de confianza
  confianza_1 <- 0.05; r_critico_1 <- qnorm(1-confianza_1)
  confianza_2 <- 0.02; r_critico_2 <- qnorm(1-confianza_2)
  confianza_3 <- 0.01; r_critico_3 <- qnorm(1-confianza_3)
  
  # Weights
  portafolio <- cant_suby_a * precio_suby_a + cant_suby_b * precio_suby_b
  
  ponderacion_a <- (cant_suby_a * precio_suby_a)/portafolio
  ponderacion_b <- (cant_suby_b * precio_suby_b)/portafolio
  
  # Retorno y Varianza portafolio
  retorno_esperado_portafolio <- ponderacion_a * rendimiento_anual_a + ponderacion_b * rendimiento_anual_b
  
  varianza_portafolio <- ponderacion_a^2*varianza_a + ponderacion_b^2*varianza_b + 2*ponderacion_a*ponderacion_b*covarianza_ab
  desvio_portafolio <- sqrt(varianza_portafolio)
  
# Calculo del var.
  # Var Absoluto  
    # W0 * -rc*
  var_absoluto <- r_critico_1 * portafolio * desvio_portafolio * sqrt(tiempo)
  
  # Var relativo
    # -W0 * (Zc * Desvio)
  var_relativo_a <- r_critico_1 * desvio_portafolio * portafolio

  # Component Var
    # Wi * Bi * VAR = Var Component
    # Bi = cov(ri;rp)/W
    # cov(ri;rp) = (w1*r1 + w2*rp) / var_p
  B1 <- (ponderacion_a * varianza_a + ponderacion_b * covarianza_ab)/varianza_portafolio
  B2 <- (ponderacion_a * covarianza_ab + ponderacion_b * varianza_b)/varianza_portafolio
  
  component_var_a <- var_absoluto * B1 * ponderacion_a
  component_var_b <- var_absoluto * B2 * ponderacion_b
  
  
  # Marginal Var
    # Var/W * Bi = Var Marginal
  
  Var_marginal_a <- var_absoluto * B1/portafolio
  Var_marginal_b <- var_absoluto * B2/portafolio
  
  # Var incremental
    # Var Incremental = Var Marginal * Precio accion * nueva cantidad
    # sumo 100 de cantidad en A
  cant_suby_a <- 2200
  
  Var_inc_a <- Var_marginal_a * precio_suby_a * 100 # El var individual aumentaria 14.35
  
# De manera exacta:
  cant_suby_a <- 2200
  cant_suby_b <- 5000
  precio_suby_a <- 5.46
  precio_suby_b <- 3.25
  rendimiento_anual_a <- 0.15
  rendimiento_anual_b <- 0.17
  varianza_a <- 0.1
  varianza_b <- 0.11
  covarianza_ab <- 0.052
  
  tiempo <- 1/252
  
  # Niveles de confianza
  confianza_1 <- 0.05; r_critico_1 <- qnorm(1-confianza_1)
  confianza_2 <- 0.02; r_critico_2 <- qnorm(1-confianza_2)
  confianza_3 <- 0.01; r_critico_3 <- qnorm(1-confianza_3)
  
  # Weights
  portafolio <- cant_suby_a * precio_suby_a + cant_suby_b * precio_suby_b
  
  ponderacion_a <- (cant_suby_a * precio_suby_a)/portafolio
  ponderacion_b <- (cant_suby_b * precio_suby_b)/portafolio
  
  # Retorno y Varianza portafolio
  retorno_esperado_portafolio <- ponderacion_a * rendimiento_anual_a + ponderacion_b * rendimiento_anual_b
  
  varianza_portafolio <- ponderacion_a^2*varianza_a + ponderacion_b^2*varianza_b + 2*ponderacion_a*ponderacion_b*covarianza_ab
  desvio_portafolio <- sqrt(varianza_portafolio)
  
  # Calculo del var.
  # Var Absoluto  
  # W0 * -rc*
  var_absoluto_nuevo <- r_critico_1 * portafolio * desvio_portafolio * sqrt(tiempo)
  
  # Diferencia 
  var_absoluto_nuevo - var_absoluto
  error_de_estimacion <- var_absoluto_nuevo - var_absoluto - Var_inc_a
  
# Ejemplo PPT ---- No terminado
rm(list=ls())

  # Parametros
  Acciones <- c("A","B","C")
  Cantidad <- c(300,200,250)
  Precio <- c(10,40,20)
  Volatilidad_anual <- c(0.25,0.15,0.20)
  Tiempo <- 10/252
  
  tabla <- tibble(Acciones,Cantidad,Precio,Volatilidad_anual)
  
  # Correlaciones
  
  rhos <- matrix(c(1, 0.7, 0.5,
                   0.7, 1, 0.6,
                   0.5, 0.6, 1), nrow = 3)
  
  matriz_cov <- diag(Volatilidad_anual) %*% rhos %*% diag(Volatilidad_anual)
  
  # Matriz varianza - Covarianza - Otra Forma
  
  matriz_cov <- matrix(NA,ncol = 3,nrow = 3)
  
  for (i in 1:3) {
    for (j in 1:3) {
      if (i == j) {
        matriz_cov[i,j] <- Volatilidad_anual[i]^2  
      } else if (i < j){
        matriz_cov[i,j] <- Volatilidad_anual[i]*Volatilidad_anual[j]*rhos[i,j] 
      } else {
        matriz_cov[i,j] <- Volatilidad_anual[i]*Volatilidad_anual[j]*rhos[i,j]
      }
    }
  }
  
  # Calculos para el VAR
  zc <- qnorm(1-0.01) # Z Critico
  h <- sqrt(10/252) # Tiempo
  W <- Cantidad * Precio
  WT <- sum(W)
  Ponderaciones_activos <- W/WT

  # Calcular la varianza del portafolio
  # Varianza del portafolio = (w1^2 * Varianza1) + (w2^2 * Varianza2) + (w3^2 * Varianza3) + 2 * (w1 * w2 * Covarianza1_2 + w1 * w3 * Covarianza1_3 + w2 * w3 * Covarianza2_3)
  varianza_portafolio <- t(matrix(Ponderaciones_activos)) %*% matriz_cov %*% matrix(Ponderaciones_activos)
  volatilidad_portafolio <- sqrt(varianza_portafolio)
  
  # Calculo VAR
  VaR <- WT*h*zc*volatilidad_portafolio
  
  # Var Individual
  Var_individual <- W * Volatilidad_anual * zc * h 
  
  # Component Var
  betas <- t(Ponderaciones_activos %*% matriz_cov/as.double(varianza_portafolio))
  
  component_var <- t(VaR %*% t(betas * Ponderaciones_activos))
  sum(component_var) # Chequeo sumatoria = Var 
  
  #Var Marginal
  Var_marginal <- t((VaR %*% t(betas))/WT)
 
  porcentaje_var <- component_var/as.double(VaR)
 
  tibble(Acciones,betas,Var_marginal,component_var,porcentaje_var)
# Benficio verificacion sum(var_individuales) - VaR
# Revisar component VAR - Desvio de cada activo y como influye en el VAR.
  
# Aula 430
  
  