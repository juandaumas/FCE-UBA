## Riesgo de Tasa y Liquidez

rm(list=ls())
graphics.off()

# Librerias
library(tidyverse)
library(ggplot2)

# Gaps de Liquidez
t <- c(1,2,3,4,5)
Activos <- c(900,700,650,500,300)
am_activos <- c(100,200,50,150,200)
Pasivos <- c(800,500,400,350,100)
am_pasivos <- c(200,300,100,50,250)

GAP <- Activos-Pasivos
GAP
Gap_mg <- am_pasivos - am_activos


library(dplyr)

# Datos de ejemplo
activos <- c(100000, 150000, 120000, 80000)
pasivos <- c(50000, 70000, 90000, 60000)

# Crear un dataframe con los datos
df <- data.frame(Activos = activos, Pasivos = pasivos)

# Calcular el gap de liquidez
df <- df %>% mutate(Gap = Activos - Pasivos)

# Imprimir el dataframe resultante
print(df)

#El GAP de liquidez es una medida que se utiliza en la gestión de riesgos en las instituciones financieras para evaluar la exposición a cambios en las tasas de interés. El GAP se calcula comparando los activos y pasivos sensibles a las tasas de interés en un determinado periodo de tiempo.

#Si el GAP es positivo, significa que los activos sensibles a las tasas de interés (como préstamos a tasa variable) superan a los pasivos sensibles a las tasas de interés (como depósitos a tasa fija). En este caso, si las tasas de interés suben, los activos generarán más ingresos que los pasivos, lo que beneficia a la institución financiera.

#Por otro lado, si el GAP es negativo, indica que los pasivos sensibles a las tasas de interés superan a los activos sensibles a las tasas de interés. Si las tasas de interés bajan, los costos de los pasivos disminuirán más rápidamente que los ingresos generados por los activos, lo cual perjudica a la institución financiera.

#En resumen:
  
#Un GAP positivo implica que un aumento en las tasas de interés beneficiará a la institución financiera.
#Un GAP negativo implica que una disminución en las tasas de interés perjudicará a la institución financiera.
#Cabe destacar que el riesgo de tasa está relacionado con la volatilidad y los cambios en las tasas de interés. Por lo tanto, la gestión adecuada del GAP de liquidez es esencial para minimizar el riesgo de tasa y proteger la rentabilidad de las instituciones financieras.
