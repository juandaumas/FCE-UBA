# 900496 - Juan Daumas - Segundo Parcial BAIF

library(quantmod)
library(tidyverse)

rm(list=ls())
graphics.off()
cat("\014")

lambda = 0.94

#Datos de los precios de las acciones
getSymbols(c("TS","GGAL","NKE"),from = "2021-01-01",to = "2023-12-01")

#DF para el conjunto de datos de precios de cierre
sum(!(index(TS) == index(GGAL)))
sum(!(index(TS) == index(NKE)))
stock_data = tibble(date = index(TS),
                    TS = as.numeric(TS$TS.Close),
                    GGAL = as.numeric(GGAL$GGAL.Close),
                    NKE = as.numeric(NKE$NKE.Close))

# Rendimientos, desvío muetsral -----------------------
R_TS = log(stock_data$TS[2:nrow(stock_data)]/stock_data$TS[1:(nrow(stock_data)-1)])
R_GGAL = log(stock_data$GGAL[2:nrow(stock_data)]/stock_data$GGAL[1:(nrow(stock_data)-1)])
R_NKE = log(stock_data$NKE[2:nrow(stock_data)]/stock_data$NKE[1:(nrow(stock_data)-1)])
stock_data = stock_data %>%
  mutate(R_TS = c(NA,R_TS),
         R_GGAL = c(NA,R_GGAL),
         R_NKE = c(NA,R_NKE))
sig =  c(TS = sd(R_TS)*sqrt(252),GGAL = sd(R_GGAL)*sqrt(252),NKE = sd(R_NKE)*sqrt(252))

# EWMA -------
#Se asume que n es lo suficientemente grande como para no realizar una correción al ponderador.
n = (nrow(stock_data)-1)
  #TS
aux = 0 
for(t in (1:(nrow(stock_data)-1))){
  aux <- aux + lambda^(t-1)*stock_data$R_TS[nrow(stock_data)-t+1]^2
};rm(t)
sigma_TS_EWMA = sqrt(252*(1-lambda)*aux)
sigma_TS_EWMA
rm(aux)
#GGAL
aux = 0 
for(t in (1:(nrow(stock_data)-1))){
  aux <- aux + lambda^(t-1)*stock_data$R_GGAL[nrow(stock_data)-t+1]^2
};rm(t)
sigma_GGAL_EWMA = sqrt(252*(1-lambda)*aux)
sigma_GGAL_EWMA
rm(aux)
#NKE
aux = 0 
for(t in (1:(nrow(stock_data)-1))){
  aux <- aux + lambda^(t-1)*stock_data$R_NKE[nrow(stock_data)-t+1]^2
};rm(t)
sigma_NKE_EWMA = sqrt(252*(1-lambda)*aux)
sigma_NKE_EWMA
rm(aux)
#Los tres
sig_EWMA = c(TS = sigma_TS_EWMA,GGAL = sigma_GGAL_EWMA,NKE = sigma_NKE_EWMA)

sig_EWMA
sig 
cat("Las volatilidades calculadas mediante el uso de medias móviles ponderadas exponencialmente difieren de las muestrales en cuanto a la ponderación de los datos recientes. En el caso de la muestral, los datos tienen todo el mismo peso, mientras que bajo EWMA se reconoce que los datos más recientes son más representativos de las condiciones actuales del mercado.")

# Correlaciones y var-cov -------------------
#misma ponderacion
R_mat = cbind(R_TS,R_GGAL,R_NKE)
rho_matrix = cor(R_mat)
varcov = sig * t(sig*rho_matrix)

#con ponderación exponencial
aux = 0
for (t in 1:n) {
  aux = aux + lambda^(t - 1) * R_mat[n - t + 1, ] %*% t(R_mat[n - t + 1, ])
}
covar_EWMA = 252 * (1 - lambda) / (1 - lambda^n) * aux

diag_inv_sqrt <- diag(1 / sqrt(diag(covar_EWMA)))
rho_mat_EWMA <- diag_inv_sqrt %*% covar_EWMA %*% diag_inv_sqrt

# Muestra la matriz de correlación EWMA
print(rho_mat_EWMA)


