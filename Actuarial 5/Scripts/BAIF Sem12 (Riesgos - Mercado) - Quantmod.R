#   Clase 28-082
rm(list=ls())
graphics.off()
# Ccargar paquetes necesarios

library(quantmod)
library(ggplot2)
library(tidyverse)

# Definir el rango de fechas
inicio <- as.Date("2023-04-01")
fin <- as.Date("2023-06-30")

# Descargar precios de las acciones de Apple e IBM
getSymbols(c("AAPL", "IBM"), from = inicio, to = fin)

# Obtener los precios de cierre 
precios <- merge(Cl(AAPL), Cl(IBM)) 

# Crear un dataframe con los precios
df_precios <- data.frame(Date = index(precios), coredata(precios))

# Calcular los rendimientos logarítmicos
rendimientos <- data.frame(Date = df_precios$Date)

for (i in 2:ncol(df_precios)) {
  rendimientos[, i] <- c(0,diff(log(df_precios[, i]))) # Agrego un 0 en la primer linea. Despues lo borro para sacar sd
}

colnames(rendimientos) <- c("Date","Rend_diario_AAPL","Rend_diario_IBM")
rendimientos <- rendimientos[-1,] #Remuevo primer linea con 0

rendimientos_aapl <- rendimientos[,2]
rendimientos_IBM <- rendimientos[,3]
# Calcular el desvío estándar de cada acción
sd_APPL_anual <- sd(rendimientos_aapl) * sqrt(252)
sd_IBM_anual <- sd(rendimientos_IBM) * sqrt(252)

# Otra forma de sacar el desvio.
desvio_std <- apply(rendimientos[, -1], 2, sd) * sqrt(252) # Anualizado 


# Calcular la EWMA -- Se calcula para toda la serie - El ultimo valor es el que interesaria
lambda <- 0.94  
var_ewma <- numeric(length(rendimientos_aapl))  # Variable para almacenar las varianzas EWMA
volatility_ewma <- numeric(length(rendimientos_aapl))  # Variable para almacenar la volatilidad EWMA

var_ewma[1] <- var(rendimientos_aapl)  # La primera varianza EWMA es igual a la varianza de los rendimientos logarítmicos

for (i in 2:length(rendimientos_aapl)) {
  var_ewma[i] <- lambda * var_ewma[i - 1] + (1 - lambda) * rendimientos_aapl[i - 1]^2
  volatility_ewma[i] <- sqrt(var_ewma[i])
}

# Visualizar los resultados
# Crear un data frame con los rendimientos logarítmicos y la volatilidad EWMA
data <- data.frame(log_returns = rendimientos_aapl, volatility_ewma = volatility_ewma)



# Comparo valores de SD y EWMA
std_dev <- sd(rendimientos_aapl)

# Crear un data frame con los rendimientos logarítmicos, la volatilidad EWMA y el desvió estándar
data <- data.frame(log_returns = rendimientos_aapl, volatility_ewma = volatility_ewma, std_dev = std_dev)

# Graficos

ggplot(df_precios, aes(x = Date)) +
  geom_line(aes(y = AAPL.Close, color = "Apple")) +
  geom_line(aes(y = IBM.Close, color = "IBM")) +
  labs(x = "Fecha", y = "Precio ajustado de cierre", color = "Acciones") +
  scale_color_manual(values = c("Apple" = "blue", "IBM" = "red")) +
  theme_bw()

# Graficar la volatilidad EWMA
ggplot(data, aes(x = index(data), y = volatility_ewma)) +
  geom_line() +
  labs(x = "Fecha", y = "Volatilidad EWMA") +
  ggtitle("Volatilidad EWMA de la serie de tiempo")

# Graficar la volatilidad EWMA y el desvió estándar
ggplot(data, aes(x = index(data))) +
  geom_line(aes(y = volatility_ewma, color = "Volatilidad EWMA")) +
  geom_hline(aes(yintercept = std_dev, color = "Desvió estándar")) +
  labs(x = "Fecha", y = "Volatilidad") +
  ggtitle("Comparación de Volatilidad EWMA y Desvió Estándar") +
  scale_color_manual(values = c("Volatilidad EWMA" = "blue", "Desvió estándar" = "red")) +
  theme(legend.position = "top")

# VAR
cantidad_aapl <- 100
periodo <- sqrt(10)
portafolio <- cantidad_aapl * last(df_precios$AAPL.Close)
VaR <- portafolio * sd(rendimientos_aapl) * periodo 
VaR_anual <- portafolio * sd_APPL_anual * sqrt(10/252)

# Ejemplo de cartera
cantidad_aapl <- 100
cantidad_ibm <- 50
precio_aapl <- last(df_precios$AAPL.Close)
precio_ibm <- last(df_precios$IBM.Close)
portafolio <- cantidad_aapl * precio_aapl + cantidad_ibm * precio_ibm

# Ponderacion cartera
wa <- (cantidad_aapl * precio_aapl)/portafolio
wb <- (cantidad_ibm * precio_ibm)/portafolio

# Correlacion con desviacion estandar
correlacion_ab <- cor(rendimientos_aapl,rendimientos_IBM)
covarianza_ab <-  sd_APPL_anual * sd_IBM_anual * correlacion_ab

varianza_portafolio <- wa^2 * sd_APPL_anual^2 + wb^2 * sd_IBM_anual^2 + 2*wa*wb*covarianza_ab

# Correlacion con metodo EWMA 

lambda <- 0.94  
var_ewma_aapl <- numeric(length(rendimientos_aapl))  # Variable para almacenar las varianzas EWMA
volatility_ewma_aapl <- numeric(length(rendimientos_aapl))  # Variable para almacenar la volatilidad EWMA

var_ewma_ibm <- numeric(length(rendimientos_aapl))  # Variable para almacenar las varianzas EWMA
volatility_ewma_ibm <- numeric(length(rendimientos_aapl))  # Variable para almacenar la volatilidad EWMA

var_ewma_aapl[1] <- var(rendimientos_aapl)  # La primera varianza EWMA es igual a la varianza de los rendimientos logarítmicos
var_ewma_ibm[1] <- var(rendimientos_IBM)  # La primera varianza EWMA es igual a la varianza de los rendimientos logarítmicos

for (i in 2:length(rendimientos_aapl)) {
  var_ewma_aapl[i] <- lambda * var_ewma_aapl[i - 1] + (1 - lambda) * rendimientos_aapl[i - 1]^2
  volatility_ewma_aapl[i] <- sqrt(var_ewma_aapl[i])
}

for (i in 2:length(rendimientos_IBM)) {
  var_ewma_ibm[i] <- lambda * var_ewma_ibm[i - 1] + (1 - lambda) * rendimientos_IBM[i - 1]^2
  volatility_ewma_ibm[i] <- sqrt(var_ewma_ibm[i])
}

cov_ab_ewma <- numeric(length(rendimientos_aapl))

for (i in 2:length(rendimientos_aapl)) {
  cov_ab_ewma[i] <- lambda * cov_ab_ewma[i - 1] + (1 - lambda) * rendimientos_IBM[i - 1]*rendimientos_AAPL[i - 1]
}


volatility_ewma_aapl_anual <- last(volatility_ewma_aapl) * sqrt(252)
volatility_ewma_ibm_anual <- last(volatility_ewma_ibm) * sqrt(252)

covarianza_ab_ewma <- volatility_ewma_aapl_anual * volatility_ewma_ibm_anual * correlacion_ab

varianza_portafolio_ewma <- wa^2 * volatility_ewma_aapl_anual^2 + wb^2 * volatility_ewma_ibm_anual^2 + 2*wa*wb*covarianza_ab_ewma
