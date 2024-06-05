## Fra y Swaps
rm(list())

#######################################
# V = N * (K - R) * t / (1 + K * t)
######################################

#   V es el valor presente del FRA
#   N es el valor nominal del contrato
#   K es la tasa de interés acordada en el contrato (también conocida como tasa de FRA)
#   R es la tasa de referencia para el período del contrato
#   t es el tiempo restante hasta la fecha de inicio del contrato expresado en años.


# Valuacion de FRA
# Ejemplo Hull

year <- c(1,2,3,4,5)

zero_rate <- c(0.03,0.04,0.046,0.05,0.053)

Tasa_forward <- function(tasas_efectivas, inicio, fin, continua = FALSE) {
  if (!is.numeric(inicio) || !is.numeric(fin)) {
    return("Inicio y fin tienen que ser números")
  } else {
    if (continua == FALSE) {
      f_rate <- (1 + tasas_efectivas[fin])^fin / ((1 + tasas_efectivas[inicio])^inicio) - 1
      return(f_rate)
    } else {
      f_rate <- (exp(tasas_efectivas[fin] * fin) / exp(tasas_efectivas[inicio] * inicio)) - 1
      return(f_rate)
    }
  }
}

tf_1_2 <- Tasa_forward(tasas_efectivas = zero_rate, inicio = 1, fin = 2, continua = F)
tf_1_2_T <- Tasa_forward(tasas_efectivas = zero_rate, inicio = 1, fin = 2, continua = T)
tf_2_3 <- Tasa_forward(tasas_efectivas = zero_rate, inicio = 2, fin = 3, continua = F)
tf_3_4 <- Tasa_forward(tasas_efectivas = zero_rate, inicio = 3, fin = 4, continua = F)
tf_4_5 <- Tasa_forward(tasas_efectivas = zero_rate, inicio = 4, fin = 5, continua = F)


# Fra Value

VN <- 100000000
tasa_fra <- 0.06 # TNA 
tasa_fra_continua <- exp(0.06)- 1 # Continua

FD_annual <-  (1+zero_rate[2])^-2
FD_continua <-  exp(-zero_rate[2]*2)

Valor_FRA_anual <- VN*(tasa_fra - tf_1_2_T)*FD_annual
Valor_FRA_continua <- VN*(tasa_fra_continua - tf_1_2_T)*FD_continua

## Swaps 

Periodo <- c(1,2,3,4)
Precio_cupon_cero <- c(945,890,835,785)
FD <- Precio_cupon_cero/1000
TasaSwap <- (1 - FD[4])/sum(FD)

## Ejemplos Swap PPT

# Preguntar para q se usa el 10%

VN <- 100000000
n <- 0.25
tasa_fija <- 0.10/4
tasa_spot_trim <- 0.12/4
tasa_spot_actual_trim <- 0.118/4

Valor_swap <- BV - BF

Flujo_Fondos <- function(vn, tasa, periodo) {
  if (!is.vector(periodo)) {
    warning("El argumento 'periodo' debe ser un vector.")
  }
  if (!is.numeric(tasa) || any(tasa < 0 | tasa > 1)) {
    warning("La tasa debe ser un valor decimal entre 0 y 1.")
  }
  
  Flujos <- rep(0, length(periodo))
  for (i in 1:length(periodo)) {
    if (i != length(periodo)) {
      Flujos[i] <- vn * tasa 
    } else {
      Flujos[i] <- vn * tasa + vn
    }
  }
  return(Flujos)
}

FF <- Flujo_Fondos(vn = 100, tasa = tasa_fija, periodo = c(2/3,5/3,8/3,11/3,14/3))
N <- c(2/3,5/3,8/3,11/3,14/3)

for(i in 1:length(N)){
  FDs[i] <- (1+0.10/4)^-N[i]
}
FDs

suma_producto <- function(v1, v2) {
  if (length(v1) != length(v2)) {
    stop("Los vectores no tienen la misma longitud")
  }
  return(sum(v1 * v2))
}

suma_producto(FF,FDs)
