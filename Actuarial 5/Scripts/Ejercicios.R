########## EJERCICIOS HEDGE FUTUROS - COBERTURA CON FUTUROS ######################
# Ejemplo del Hull pagina 61

rm(list=ls())

SP_Index <- 1000
Futuro_SP <- 1010
VN <- 5050000
tlr <- 0.04
retorno_indice <- 0.01
Beta <- 1.5
Multiplicador <- 250

Valor_Futuro <- Futuro_SP * Multiplicador

Cant_Futuros <- Beta * (VN/Valor_Futuro)

    # en 3 meses
Futuro_SP_t3 <- 902
SP_Index_t3 <- 900

Ganancia <- ((Futuro_SP*Cant_Futuros)-(Futuro_SP_t3 * Cant_Futuros)) * Multiplicador

retorno_indice_3meses <- retorno_indice/4
retorno_indice_real <- ((SP_Index_t3-SP_Index)/SP_Index)+retorno_indice_3meses
tlr_3meses <- tlr/4

# CAPM
# Retorno_portafolio <- Beta * (Retorno Mercado - TLR) + TLR
Retorno_portafolio <- Beta * (retorno_indice_real - tlr_3meses) + tlr_3meses

Valor_esperado_portafolio <- VN * (1+Retorno_portafolio)

# Performance de la cobertura
rm(list=ls())

Valor_indice_t3 <- c(900,950,1000,1050,1100)
Valor_futuro_hoy <- c(rep(1010,5))

Valor_futuro_t3 <- c(902,952,1003,1053,1103)

Ganancia <- NULL
for (i in 1:length(Valor_indice_t3)){
  Ganancia[i] <- ((Valor_futuro_hoy[i] * Cant_Futuros) - (Valor_futuro_t3[i] * Cant_Futuros)) * Multiplicador
}

retorno_indice_real <- NULL
for (i in 1:length(Valor_indice_t3)){
  retorno_indice_real[i] <- ((Valor_indice_t3[i]-SP_Index)/SP_Index)+retorno_indice_3meses
}
retorno_indice_real

Retorno_portafolio <- NULL
for (i in 1:length(Valor_indice_t3)){
  Retorno_portafolio[i] <- Beta * (retorno_indice_real[i] - tlr_3meses) + tlr_3meses
}
Retorno_portafolio

Valor_esperado_portafolio <- NULL
for (i in 1:length(Valor_indice_t3)){
  Valor_esperado_portafolio[i] <- VN * (1+Retorno_portafolio[i])
}

Valor_total_posicion <- NULL
for (i in 1:length(Valor_indice_t3)){
  Valor_total_posicion[i] <- Ganancia[i]  + Valor_esperado_portafolio[i]
}

############ Ejercicios HULL pagina 68 ##########
# 3.6
rm(list=ls())
desvio_commodity <- 0.65
desvio_futuro <- 0.81
rho <- 0.8

ratio_optimo_de_cobertura <- rho*(desvio_commodity/desvio_futuro)

# 3.7
rm(list=ls())

VN_Portafolio <- 20000000
Beta <- 1.2
Futuro_indice <- 1080
Multiplicador <- 250

cantidad_futuros <- Beta * (VN_Portafolio/(Futuro_indice * Multiplicador))

# con 0.6 de beta esperado
cantidad_futuros <- 0.6 * (VN_Portafolio/(Futuro_indice * Multiplicador))

# 3.8
# July September March

# 3.16
rm(list=ls())

desvio_spot <- 1.2
desvio_fut <- 1.4
ro <- 0.7
vn <- 200000
multiplicador <- 40000

h <- ro * (desvio_spot/desvio_fut)

cantidad_futuros <- vn/multiplicador


# 3.23
rm(list=ls())
Spot_price_change <- c(0.5,0.61,-0.22,-0.35,0.79,0.04,0.15,0.7,-0.51,-0.41)

Future_price_change <- c(0.56,0.63,-0.12,-0.44,0.6,-0.06,0.01,0.8,-0.56,-0.46)

Media_spot <- mean(Spot_price_change)
Media_futuro <- mean(Future_price_change)

desvio_spot <- sqrt((sum(Spot_price_change^2) - length(Spot_price_change)*Media_spot^2)/(length(Spot_price_change)-1))
desvio_futuro <- sqrt((sum(Future_price_change^2) - length(Future_price_change)*Media_futuro^2)/(length(Future_price_change)-1))
h <-desvio_spot/desvio_futuro

# 3.24
rm(list=ls())
VN <- 100000000
beta_portafolio <- 1.2
beta <- 0.5
precio_futuro <- 1000
multiplicador <- 250

cantidad_futuros <- (beta_portafolio-beta)*(VN/(precio_futuro*multiplicador))

VN <- 100000000
beta_portafolio <- 1.2
beta <- 1.5
precio_futuro <- 1000
multiplicador <- 250

cantidad_futuros <- (beta_portafolio-beta)*(VN/(precio_futuro*multiplicador))

## Opciones - Demostracion Call no es mejor ejercerlo antes
#   c >= S0 - K*e^-rt
#   C >= c
#   C >= S0 - K*e^-rt

## Tasas de Interes
rm(list=ls())
interes <- 0.1 #TNA
m <- 4 # Periodos
im <- interes/m #tasa periodica o tasa efectiva mensual
TEA <- (1+im)^4-1 #Tasa efectiva anual

#     FRA
# tomo deuda por t1 -> VN
# Invierto VN a t2

# Deuda hoy a t2
# invierto a t1

#     BONOS 
# Ejemplo
rm(list=ls())
n <- 3
r <- 0.10 # Tasa continua semianual
tir <- 0.12
vn <- 100

t <- seq(0.5,3,by = 0.5)
cupon <- c(rep(5,length(t)-1),5+vn)
FD <- exp(-t*tir)
B <- sum(FD*cupon)

data.frame(t,cupon,FD)
duration <- sum(t*FD*cupon)/B

delta_i <- 0.001 #10 puntos basicos

variacion_bono <- delta_i*duration*-B 
B+variacion_bono

modified_dur <- duration/(1+tir)

convexidad <- sum(t^2*FD*cupon)/B

# Ejercicios Bonos
rm(list=ls())
#  4.24 - A
tasa <- 0.05 #TNS con capitalizacion semestral
TEA <-(1+0.05)^2-1 #TEA
TNA <- 0.05*2 #TNA con capitalizacion semestral -> im = j/m 
#  4.24 - B
tasa <- 0.05 #TNS con capitalizacion semestral
TEM <- 0.05*1/6
TNA <- TEM * 12
TEA <- (1+TEM)^12-1

#  4.24 - C
tasa <- 0.05 #TNS con capitalizacion semestral

r_cont = log(1+tasa*2)/2

# SWAPS

