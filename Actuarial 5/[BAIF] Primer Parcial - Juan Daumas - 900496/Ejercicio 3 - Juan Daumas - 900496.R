# ---- Ejercicio 3 - Primer Parcial - Juan Daumas - 900496

rm(list=ls())
library(tidyverse)
library(ggplot2)

#A)

Rendimiento_acciones <- 0.25
Desvio_acciones <- 0.20
Rendimiento_bonos <- 0.10
Desvio_bonos <- 0.5

ponderacion_activo <-seq(0,1,by = 0.001)
ponderacion_bono <- 1-ponderacion_activo

Retorno_portafolio <- ponderacion_activo * Rendimiento_acciones + ponderacion_bono * Rendimiento_bonos

rho_1 = -1
rho_2 = -0.25
rho_3 = 0 
rho_4 = 0.25
rho_5 = 1

cov_1 <- rho_1 * Desvio_acciones * Desvio_bonos
cov_2 <- rho_2 * Desvio_acciones * Desvio_bonos
cov_3 <- rho_3 * Desvio_acciones * Desvio_bonos
cov_4 <- rho_4 * Desvio_acciones * Desvio_bonos
cov_5 <- rho_5 * Desvio_acciones * Desvio_bonos


Varianza_1_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_1
Desvio_1_portafolio <- sqrt(Varianza_1_portafolio)

Varianza_2_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_2
Desvio_2_portafolio <- sqrt(Varianza_2_portafolio)

Varianza_3_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_3
Desvio_3_portafolio <- sqrt(Varianza_3_portafolio)

Varianza_4_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_4
Desvio_4_portafolio <- sqrt(Varianza_4_portafolio)

Varianza_5_portafolio <- Desvio_acciones^2 * ponderacion_activo^2  + Desvio_bonos^2 * ponderacion_bono^2 + 2 * ponderacion_activo * ponderacion_bono * cov_5
Desvio_5_portafolio <- sqrt(Varianza_5_portafolio)


Analisis_portafolio <- data.frame(
  Pond_activo = ponderacion_activo,
  Pond_bono = ponderacion_bono,
  Rendimiento_portafolio = Retorno_portafolio,
  Desvio_p_1 = Desvio_1_portafolio,
  Desvio_p_2 = Desvio_2_portafolio,
  Desvio_p_3 = Desvio_3_portafolio,
  Desvio_p_4 = Desvio_4_portafolio,
  Desvio_p_5 = Desvio_5_portafolio
)
Analisis_portafolio

ggplot(Analisis_portafolio) + 
  geom_point(aes(x=Desvio_p_1, y=Rendimiento_portafolio,color="Rho = -1")) +
  geom_point(aes(x=Desvio_p_2, y=Rendimiento_portafolio,color="Rho = -0.25"))+
  geom_point(aes(x=Desvio_p_3, y=Rendimiento_portafolio,color="Rho = 0"))+
  geom_point(aes(x=Desvio_p_4, y=Rendimiento_portafolio,color="Rho = 0.25"))+
  geom_point(aes(x=Desvio_p_5, y=Rendimiento_portafolio,color="Rho = 1"))+
  scale_color_discrete() + 
  ggtitle("Riesgo-Retorno del Portafolio",subtitle = "Analisis para distintos coeficientes de correlacion") +
  xlab("Desviación Estándar") + ylab("Rendimiento") +labs(color = "Coeficientes de Correlación") +
  theme_classic()

# B)

TLR <- 0.05

#Cartera optima por Rendimiento

ratio_sharpe <- (Analisis_portafolio$Rendimiento_portafolio - TLR)/Analisis_portafolio$Desvio_p_3

indice_maximo <- which.max(ratio_sharpe)

Cantidad_activo_optima <- Analisis_portafolio$Pond_activo[indice_maximo]
Cantidad_bono_optima <- Analisis_portafolio$Pond_bono[indice_maximo]
Rendimiento_cartera_optima <-Analisis_portafolio$Rendimiento_portafolio[indice_maximo]
Desvio_cartera_optima <- Analisis_portafolio$Desvio_p_3[indice_maximo]

Pendiente_CAL <- max(ratio_sharpe)
Portafolio_rho3 <- NULL
Portafolio_rho3 <- data.frame(Analisis_portafolio[,c(1:3,6)])

ggplot(data = Portafolio_rho3, aes(x = Desvio_p_3, y = Rendimiento_portafolio)) +
  geom_point(aes(color = "Ponderaciones portafolio"),size = 0.5) +
  geom_point(aes(x = Desvio_cartera_optima, y = Rendimiento_cartera_optima, color = "Portafolio Optimo")) +
  geom_abline(slope = Pendiente_CAL, intercept = TLR,color = "red",linewidth=0.5) +
  labs(title = "Grafico Portafolio Optimo", subtitle = "con Capital Allocation Line", x = "Desvio", y = "Rendimiento") +
  scale_color_manual(values = c("Ponderaciones portafolio" = "black", "Portafolio Optimo" = "green")) +
  xlim(0.15, 0.30) + ylim(0.15, 0.30) +
  theme_classic() + guides(color = guide_legend(title = "Leyenda")) 

