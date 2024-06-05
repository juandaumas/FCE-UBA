### Mercado de Capitales y Teorias sobre Carteras de inversion
rm(list = ls())
library(ggplot2)

# Capital Allocation

rf <- 0.07
y <- 0.75
Rendimiento_esperado <- 0.15
Desvio_rendimiento_esperado <- 0.22

Rendimiento_cartera <- y * Rendimiento_esperado + (1-y)*rf
Rendimiento_cartera

Desvio_cartera <- y * Desvio_rendimiento_esperado
Desvio_cartera

# Carteras de riesgo optimas con distintos niveles de correlacion

Expected_return_debt <- 0.08
Expected_return_equity <- 0.13
Standard_deviation_debt <- 0.12
Standard_deviation_equity <- 0.20

# covariance_debt_equity <- 72

rho_1 = -1
rho_2 = 0
rho_3 = 0.3 
rho_4 = 1

Weight_debt <- seq(0,1,by = 0.1)
Weight_equity <- seq(1,0,by = -0.1)

Expected_return_portafolio <- Weight_debt*Expected_return_debt + Weight_equity* Expected_return_equity
Expected_return_portafolio * 100

Covarianza_rho_1 <- rho_1 * Standard_deviation_debt * Standard_deviation_equity
Covarianza_rho_2 <- rho_2 * Standard_deviation_debt * Standard_deviation_equity
Covarianza_rho_3 <- rho_3 * Standard_deviation_debt * Standard_deviation_equity
Covarianza_rho_4 <- rho_4 * Standard_deviation_debt * Standard_deviation_equity

Variance_portafolio_1 <- Weight_debt^2 * Standard_deviation_debt^2 + Weight_equity^2 * Standard_deviation_equity^2 + 2 * Weight_debt * Weight_equity * Covarianza_rho_1
Variance_portafolio_1
Deviation_portafolio_1 <- sqrt(Variance_portafolio_1)

Variance_portafolio_2 <- Weight_debt^2 * Standard_deviation_debt^2 +Weight_equity^2 * Standard_deviation_equity^2 + 2 * Weight_debt * Weight_equity * Covarianza_rho_2
Variance_portafolio_2
Deviation_portafolio_2 <- sqrt(Variance_portafolio_2)

Variance_portafolio_3 <- Weight_debt^2 * Standard_deviation_debt^2 +Weight_equity^2 * Standard_deviation_equity^2 + 2 * Weight_debt * Weight_equity * Covarianza_rho_3
Variance_portafolio_3
Deviation_portafolio_3 <- sqrt(Variance_portafolio_3)

Variance_portafolio_4 <- Weight_debt^2 * Standard_deviation_debt^2 +Weight_equity^2 * Standard_deviation_equity^2 + 2 * Weight_debt * Weight_equity * Covarianza_rho_4
Variance_portafolio_4
Deviation_portafolio_4 <- sqrt(Variance_portafolio_4)


sqrt(Variance_portafolio_1)*100

Portafolio_Distintas_Correlaciones <- data.frame(
  Wb = Weight_debt,
  We = Weight_equity,
  E_rp = Expected_return_portafolio*100,
  Dev_1 = Deviation_portafolio_1*100,
  Dev_2 = Deviation_portafolio_2*100,
  Dev_3 = Deviation_portafolio_3*100,
  Dev_4 = Deviation_portafolio_4*100
)

Portafolio_Distintas_Correlaciones

# Calcular los desvíos estándar de los distintos portafolios
Deviation_portafolio <- c(Deviation_portafolio_1, Deviation_portafolio_2, Deviation_portafolio_3, Deviation_portafolio_4)

# Obtener el índice del mínimo desvío estándar
min_index <- which.min(as.matrix(Portafolio_Distintas_Correlaciones[4:7]))

# Seleccionar los pesos correspondientes
optimal_debt_weight <- Weight_debt[min_index]
optimal_equity_weight <- Weight_equity[min_index]

# Mostrar los resultados
cat("El peso óptimo para la deuda es:", optimal_debt_weight, "\n")
cat("El peso óptimo para la renta variable es:", optimal_equity_weight, "\n")
cat("El desvío estándar óptimo es:", Deviation_portafolio[min_index], "\n")

###########################
## Queda crear la funcion #
###########################

Mtx_Retorno <- matrix( data = c(0.08,0.13) , ncol = 2)
Mtx_Desvios <- matrix( data = c(0.12,0.20), ncol = 2)
Coeficiente_correlacion <- 0.25
Pesos <- matrix(data = c(seq(0,1,by = 0.1),seq(1,0,by = -0.1)) , ncol = 2)

Retorno_portafolio <-  Pesos[,1]*Mtx_Retorno[1] + Pesos[,2] * Mtx_Retorno[2]
Retorno_portafolio_100 <- Retorno_portafolio * 100

Covarianza_rho_1 <- Coeficiente_correlacion * Mtx_Desvios[1] * Mtx_Desvios[2]

Varianza_portafolio <-  Pesos[,1]^2 * Mtx_Desvios[1]^2 + Pesos[,2]^2 * Mtx_Desvios[2]^2 + 2 * Pesos[,1] * Pesos[,2] * Covarianza_rho_1

Desvio_portafolio_100 <- sqrt(Varianza_portafolio)*100


df_Portafolio <- data.frame(
  Wb = Pesos[,1],
  We = Pesos[,2],
  E_rp = Retorno_portafolio_100,
  Desvio = Desvio_portafolio_100)

df_Portafolio

min_index <- which.min(as.matrix(df_Portafolio[,4]))

Peso_optimo_deuda <- Pesos[min_index,1]
Peso_optimo_accion <- Pesos[min_index,2]
e_rp_optimo <- df_Portafolio$E_rp[min_index]
desvio_optimo <- df_Portafolio$Desvio[min_index]

# CAL
retorno_a <- 0.08
rf <- 0.07
rf_100 <- rf * 100
desvio_a <- 0.12
Pend <- (retorno_a - rf) / desvio_a


# Crear el objeto de trazado
p <- ggplot(data = df_Portafolio, aes(x = Desvio, y = E_rp))

# Agregar puntos para todos los portafolios
p + geom_point(aes(color = "Indicadores"), size = 1.5,shape = 16) +
  
  # Agregar el punto rojo para la cartera óptima
  geom_point(aes(desvio_optimo, e_rp_optimo, color = "Cartera óptima"), 
             size = 3, shape = 16) +
  
  # Agregar etiquetas de los ejes y título
  labs(x = "Desvíos del portafolio", y = "Retornos del portafolio", 
       title = "Frontera de eficiencia") +
  
  # Modificar la leyenda del color
  scale_color_discrete(name = "Indicadores", 
                       labels = c("Cartera óptima","Portafolio"))

##########################
# Crear el objeto de trazado
p <- ggplot(data = Portafolio_Distintas_Correlaciones, aes(x = Dev_1    , y = E_rp))

# Agregar puntos para todos los portafolios
p  + geom_point(aes(x=Dev_1,y=E_rp))+
  geom_point(aes(x=Dev_2,y=E_rp))+
  geom_point(aes(x=Dev_3,y=E_rp))+
  geom_point(aes(x=Dev_4,y=E_rp))
  
  # Agregar el punto rojo para la cartera óptima
  geom_point(aes(desvio_optimo, e_rp_optimo, color = "Cartera óptima"), 
             size = 3, shape = 16) +
  
  # Agregar etiquetas de los ejes y título
  labs(x = "Desvíos del portafolio", y = "Retornos del portafolio", 
       title = "Frontera de eficiencia") +
  
  # Modificar la leyenda del color
  scale_color_discrete(name = "Indicadores", 
                       labels = c("Cartera óptima","Portafolio"))



################## creando con GPT############

# Definir variables para el rendimiento y desviación estándar de acciones y bonos
Rendimiento_acciones <- 0.2
Desvio_acciones <- 0.18
Rendimiento_bonos <- 0.12
Desvio_bonos <- 0.15

# Definir vectores de ponderación para el activo y el bono
ponderacion_activo <- seq(0, 1, by = 0.1)
ponderacion_bono <- rev(ponderacion_activo)

# Definir función para calcular la desviación estándar del portafolio con diferentes valores de correlación
calcular_desviacion <- function(rho) {
  cov <- rho * Desvio_acciones * Desvio_bonos
  Varianza_portafolio <- Desvio_acciones^2 * ponderacion_activo^2 + Desvio_bonos^2 * ponderacion_bono + 2 * Desvio_bonos * Desvio_acciones * cov
  return(sqrt(Varianza_portafolio))
}

# Calcular la desviación estándar del portafolio para diferentes valores de correlación
rho <- c(-1, -0.5, 0, 0.5, 1)
desviacion_portafolio <- sapply(rho, calcular_desviacion)

# Crear un data frame para el análisis del portafolio
Analisis_portafolio <- data.frame(
  Wb = ponderacion_activo,
  We = ponderacion_bono,
  E_rp = (ponderacion_activo * Rendimiento_acciones + ponderacion_bono * Rendimiento_bonos) * 100,
  Dev_1 = desviacion_portafolio[1] * 100,
  Dev_2 = desviacion_portafolio[2] * 100,
  Dev_3 = desviacion_portafolio[3] * 100,
  Dev_4 = desviacion_portafolio[4] * 100,
  Dev_5 = desviacion_portafolio[5] * 100
)
