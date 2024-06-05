## Instrumentos Renta Fija ##
rm(list = ls())
# Calculo del precio de un bono a tasa fija


# Rendimiento de un bono
Precio_Venta <- 110
Precio_Compra <- 100
Tasa <- 0.10
Interes <- Tasa * Precio_Compra

Rendimiento <- (Precio_Venta - Precio_Compra + Interes)/Precio_Compra
Rendimiento
Rendimiento_porcentaje <- paste(format(Rendimiento * 100, nsmall = 2), "%")
Rendimiento_porcentaje

# Medidas de Rendimiento
# Tasa Cupon:

Tasa_Cupon <- sum(Cupon)/Valor_Nominal # Cupon deberia ser annual

# Curreny Yield:

Current_Yield <- sum(Cupon)/Valor_Nominal # Cupon deberia ser annual

# TIR o YTM - Yield To Maturity
# Sumatoria de los flujos de fondos * (1 + TIR)^-t


# Calculo de la tasa forward

VN <- 1000
Periodo <- c(1,2,3,4)
Precio_Cupon_Cero <- c(945,890,835,785)

Factor_Descuento <- Precio_Cupon_Cero/VN
Tasa_Efectiva_spot <- matrix()
Tasa_Efectiva_anual <- matrix()

for (i in 1:length(Periodo)) {
  Tasa_Efectiva_spot[i] <- ((1/Factor_Descuento[i])^(1/Periodo[i])) - 1 
  Tasa_Efectiva_anual[i] <- ((1 + Tasa_Efectiva_spot[i])^Periodo[i]) - 1
}

print(Tasa_Efectiva_spot)
print(Tasa_Efectiva_anual)

# Tasa Continua - Se aplica Logaritmo natural a la tasa efectiva + 1 
Tasa_continua_spot <- c()
Tasa_continua_anual <- c()

for (i in 1:length(Periodo)) {
  Tasa_continua_spot[i] <- log(Tasa_Efectiva_spot[i] + 1)
  Tasa_continua_anual[i] <- log(Tasa_Efectiva_anual[i] + 1 )
}

print(Tasa_continua_spot)
print(Tasa_continua_anual)

# Crear un data.frame con las tasas y el período como la primera columna
df_tasas <- data.frame(Periodo = Periodo,
                       Tasa_Efectiva_Spot = Tasa_Efectiva_spot,
                       Tasa_Efectiva_Anual = Tasa_Efectiva_anual,
                       Tasa_Continua_Spot = Tasa_continua_spot,
                       Tasa_Continua_Anual = Tasa_continua_anual)

print(df_tasas)

# Tasas forward Efectivas
fwd_01 <- Tasa_Efectiva_spot[1]
fwd_12 <- ((1 + Tasa_Efectiva_spot[2])^2 / (1 + Tasa_Efectiva_spot[1])^1)^(1/(2-1)) - 1
fwd_23 <- ((1 + Tasa_Efectiva_spot[3])^3 / (1 + Tasa_Efectiva_spot[2])^2)^(1/(3-2)) - 1
fwd_34 <- ((1 + Tasa_Efectiva_spot[4])^4 / (1 + Tasa_Efectiva_spot[3])^3)^(1/(4-3)) - 1

print(paste0("Tasa forward 1-2: ", round(fwd_12 * 100, 2), "%"))
print(paste0("Tasa forward 2-3: ", round(fwd_23 * 100, 2), "%"))
print(paste0("Tasa forward 3-4: ", round(fwd_34 * 100, 2), "%"))

# Tasas forward Efectivas
log(fwd_12+1)
log(fwd_23+1)
log(fwd_34+1)


#Ejemplo Valuacion de Bono a Tasa Fija

# Definir la función
suma_producto <- function(v1, v2) {
  if (length(v1) != length(v2)) {
    stop("Los vectores no tienen la misma longitud")
  }
  return(sum(v1 * v2))
}

# Ejemplo de uso
FF <- c(7, 7, 7, 107)
FD <- c(0.945, 0.89, 0.835, 0.785)
Precio_bono <- suma_producto(FF, FD)

Precio_bono

# TIR
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
Flujo_Fondos(100,0.07,c(1,2,3,4))

vn_ref <- 102.685

VAN <- function(tasa, flujos) {
  vn <- sum(flujos / (1 + tasa)^(1:length(flujos)))
  return(vn)
}


TIR <- function(flujos, vn_ref) {
  optimise(function(tasa) abs(VAN(tasa, flujos) - vn_ref), c(0, 1))$minimum
}


tir <- TIR(flujos = Flujo_Fondos(100,0.07,c(1,2,3,4)), vn_ref = 102.685)

# Duration y Convexidad

Flujos_Fondos_ejemplo <- Flujo_Fondos(100,0.07,c(1,2,3,4))

calcular_bono_con_tir <- function(FFs, Periodos, tasa_tir){
  
  # Calcular el valor presente de cada flujo de fondos
  VP <- FFs / (1 + tasa_tir)^Periodos
  
  # Calcular el precio del bono
  precio <- sum(VP)
  
  # Calcular la duración del bono
  duracion <- sum((Periodos * VP) / precio)
  
  # Calcular la duración modificada del bono
  duracion_modificada <- duracion / (1 + tasa_tir)
  
  # Calcular la convexidad del bono
  convexidad <- sum(((Periodos + Periodos^2) * VP) / ((1 + tasa_tir)^2 * precio))
  
  # Devolver una lista con los resultados
  lista_resultados <- list(precio = precio,
                           duracion = duracion,
                           duracion_modificada = duracion_modificada,
                           convexidad = convexidad)
  return(lista_resultados)
}

calcular_bono_con_precio <- function(FFs, Periodos, precio_bono){
  
  VAN <- function(tasa, FFs) {
    vn <- sum(FFs / (1 + tasa)^(1:length(FFs)))
    return(vn)
  }
  
  TIR <- function(flujos, vn_ref) {
    optimise(function(tasa) abs(VAN(tasa, flujos) - vn_ref), c(0, 1))$minimum
  }
  tir <- TIR(flujos = FFs,vn_ref = precio_bono)
  
  # Calcular el valor presente de cada flujo de fondos
  VP <- FFs / (1 + tir)^Periodos
  
  # Calcular el precio del bono
  precio <- precio_bono
  
  # Calcular la duración del bono
  duracion <- sum((Periodos * VP) / precio)
  
  # Calcular la duración modificada del bono
  duracion_modificada <- duracion / (1 + tir)
  
  # Calcular la convexidad del bono
  convexidad <- sum(((Periodos + Periodos^2) * VP) / ((1 + tir)^2 * precio))
  
  # Devolver una lista con los resultados
  lista_resultados <- list(tir = tir,
                           precio = precio,
                           duracion = duracion,
                           duracion_modificada = duracion_modificada,
                           convexidad = convexidad)
  return(lista_resultados)
}

calcular_bono_con_tir(FFs = Flujos_Fondos_ejemplo,
                      Periodos = c(1,2,3,4),
                      tasa_tir = 0.06218724
                      )
calcular_bono_con_precio(FFs = Flujos_Fondos_ejemplo,
                         Periodos = c(1,2,3,4),
                         precio_bono = 102.6937)


