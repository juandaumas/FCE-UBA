#### Contratos de Opción: valuación en tiempo continuo ###
rm(list=ls())
library(ggplot2)
library(tidyr)


# Movimiento Browniano

S0 <- 0
N <- 365
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
t <- seq(1,N)

for (i in 2:N){
  precios[i] <- precios[i-1] + rnorm(1)*sqrt(delta_t)
}
data_precios <- data.frame(t,precios)

ggplot(data_precios) +
  geom_line(aes(x=t,y=precios)) + 
  theme_bw() +
  ggtitle(label = 'Movimiento Browniano', 'Proceso sin tenencia') + xlab("Tiempo") + ylab("Precio")

m = 10000
precios_2 <- matrix(NA,ncol=N,nrow=m)
precios_2[,1] <- S0
t <- 1:365
for (i in 1:m) {
  for (j in 2:N){
    precios_2[i,j] <- precios_2[i,j-1] + rnorm(1)*sqrt(delta_t)
  }  
}
# precios_2_t <- t(precios_2)
# data_frame <- data.frame(t,precios_2_t)
# data_long <- pivot_longer(data_frame, cols = -t, names_to = "serie", values_to = "valor")

# ggplot(data_long, aes(x = t, y = valor, color = serie)) +
#  geom_line() +
#  ggtitle(label = 'Movimiento Browniano', 'Proceso sin tenencia') + xlab("Tiempo") + ylab("Zt")+
#  theme(legend.title = element_blank(),legend.position = "none") 

plot(precios_2[1,], type = "l",ylim = c(min(precios_2),max(precios_2)))
for (i in 2:m) {
  lines(precios_2[i,], type = "l", col = runif(1,min = 1,max = 20))
}

mean(precios_2[,365])
media <- NULL
for (i in 1:365) {
  media[i] <- mean(precios_2[,i])
}
lines(media,type = "l", lwd = 5, col = "black")

sd(precios_2[,365])
valores_max <- NULL
valores_min <- NULL
for (i in 1:365) {
  valores_max[i] <- media[i] + 1.965 * sd(precios_2[,i])
  valores_min[i] <- media[i] - 1.965 * sd(precios_2[,i])
}

lines(valores_max,type = "l", lwd = 5, col = "red")
lines(valores_min,type = "l", lwd = 5, col = "red")

# Movimiento Browniano Generalizado
rm(list=ls())

S0 <- 10
N <- 365
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
mu <- 0.1 #ES ANUAL
sigma <- 0.2 #ES ANUAL
t <- seq(1,N)

for (i in 2:N){
  precios[i] <- precios[i-1] + mu * delta_t + sigma*rnorm(1) * sqrt(delta_t)
}
data_precios <- data.frame(t,precios)

ggplot(data_precios) +
  geom_line(aes(x=t,y=precios)) + 
  theme_bw() +
  ggtitle(label = 'Movimiento Browniano Generalizado', 'Proceso con tenencia') + xlab("Tiempo") + ylab("Precio")

m = 10000
precios_2 <- matrix(NA,ncol=N,nrow=m)
precios_2[,1] <- S0

for (i in 1:m) {
  for (j in 2:N){
    precios_2[i,j] <- precios_2[i,j-1] + mu * delta_t + sigma*rnorm(1) * sqrt(delta_t)
  }  
}

plot(precios_2[1,], type = "l",ylim = c(min(precios_2),max(precios_2)))
for (i in 2:m) {
  lines(precios_2[i,], type = "l", col = runif(1,min = 1,max = 20))
}

mean(precios_2[,365])
media <- NULL
for (i in 1:365) {
  media[i] <- mean(precios_2[,i])
}
lines(media,type = "l", lwd = 5, col = "black")

sd(precios_2[,365])
valores_max <- NULL
valores_min <- NULL
for (i in 1:365) {
  valores_max[i] <- media[i] + 1.965 * sd(precios_2[,i])
  valores_min[i] <- media[i] - 1.965 * sd(precios_2[,i])
}

lines(valores_max,type = "l", lwd = 5, col = "red")
lines(valores_min,type = "l", lwd = 5, col = "red")


# Ejemplo de Movimiento Browniano Generalizado en una accion
rm(list=ls())

S0 <- 10
rendimiento <- 0.07
desvio <- 0.25
N <- 365
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
t <- seq(1,N)

for (i in 2:N){
  precios[i] <- precios[i-1] * exp(rendimiento * delta_t + desvio *rnorm(1) * sqrt(delta_t))
}
data_precios <- data.frame(t,precios)

ggplot(data_precios) +
  geom_line(aes(x=t,y=precios)) + 
  theme_bw() +
  ggtitle(label = 'Movimiento Browniano Generalizado', 'Accion sin dividendo') + xlab("Tiempo") + ylab("Precio")

m = 10000
precios_2 <- matrix(NA,ncol=N,nrow=m)
precios_2[,1] <- S0

for (i in 1:m) {
  for (j in 2:N){
    precios_2[i,j] <- precios_2[i,j-1] + rnorm(1)*sqrt(delta_t)
  }  
}

plot(precios_2[1,], type = "l",ylim = c(min(precios_2),max(precios_2)))
for (i in 2:m) {
  lines(precios_2[i,], type = "l", col = runif(1,min = 1,max = 20))
}

mean(precios_2[,365])
media <- NULL
for (i in 1:365) {
  media[i] <- mean(precios_2[,i])
}
lines(media,type = "l", lwd = 5, col = "black")

sd(precios_2[,365])
valores_max <- NULL
valores_min <- NULL
for (i in 1:365) {
  valores_max[i] <- media[i] + 1.965 * sd(precios_2[,i])
  valores_min[i] <- media[i] - 1.965 * sd(precios_2[,i])
}

lines(valores_max,type = "l", lwd = 5, col = "red")
lines(valores_min,type = "l", lwd = 5, col = "red")

# Lema de Ito
rm(list=ls())

# dx = a(x;t) * dt + b(x;t) * dz
# SI Xt sigue proceso de ITO ==> Gt = f(Xt,t) ==> Gt sigue un proceso de Ito

# dG = [G'x * a + G't + G^2'x^2 * b^2 ] dt + [G'x * b ] dz

# dG = ln(St)
# dz <- rnorm(1) * sqrt(delta_t)

S0 <- 20
rendimiento <- 0.1
desvio <- 0.25
N <- 365
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
t <- seq(1,N)

for (i in 2:N){
  precios[i] <- precios[i-1] * exp((rendimiento-0.5*desvio^2) * delta_t + desvio * rnorm(1) * sqrt(delta_t))
}
data_precios <- data.frame(t,precios)

ggplot(data_precios) +
  geom_line(aes(x=t,y=precios)) + 
  theme_bw() +
  ggtitle(label = 'Movimiento Browniano Generalizado', 'Accion sin dividendo') + xlab("Tiempo") + ylab("Precio")

# Movimiento Geometrico Browniano
rm(list=ls())

S0 <- 10
mu <- 0.07
sigma <- 0.25
N <- 365
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
t <- seq(1,N)

for (i in 2:N){
  precios[i] <- precios[i-1] + (mu * precios[i-1]* delta_t + sigma * precios[i-1]*rnorm(1) * sqrt(delta_t))
}
data_precios <- data.frame(t,precios)

ggplot(data_precios) +
  geom_line(aes(x=t,y=precios)) + 
  theme_bw() +
  ggtitle(label = 'Movimiento Browniano Generalizado', 'Accion sin dividendo') + xlab("Tiempo") + ylab("Precio")

# Simulacion Montercarlo
rm(list=ls())
S0 <- 20
mu <- 0.1
sigma <- 0.25
M <- 10000 #Cantidad de escenarios
N <- 100
delta_t <- 1/N
precios <- NULL
precios[1] <- S0
t <- seq(1,N)
vector <- rep(S0,M)

matriz_precios <- matrix(ncol=M,nrow=N)
matriz_precios[1,] <- vector

for(i in 1:M){
  for(j in 2:N){
    matriz_precios[j,i] <- matriz_precios[j - 1,i] * exp((mu - 0.5 * sigma^2) * delta_t + sigma * sqrt(delta_t) * qnorm(runif(1,min=0,max=1))) 
  }
} # TIENE UN ERROR PORQUE TOMA EL VALOR ANTERIOR. APLICANDO LEMA DE ITO SE SOLUCIONA

matplot(matriz_precios,type = "l",xlab = "Periodo",ylab = "Precio Subyacente")
title("Simulacion Montercarlo")

# Distribucion Log Normal Subaycente
rm(list=ls())

S0 <- 40
mu <- 0.16
sigma <- 0.2
alpha <- 0.05
alpha_dividido <- alpha/2 #Recordar dividirlo en 2. Alpha seria 0.05 pero 
confianza <- 1 - alpha 

N <- 180
t_anual <- N/365
delta_t <- 1/N # Pasos (diarios)


Esperanza <- log(S0) + (mu - 0.5*sigma^2) * t_anual
Desvio <- sigma^2 * t_anual
intervalo_confianza <- qnorm(confianza)

precio_min <- Esperanza - intervalo_confianza * sqrt(Desvio)
precio_max <- Esperanza + intervalo_confianza * sqrt(Desvio)

precio_min_exp <- exp(precio_min)
precio_max_exp <- exp(precio_max)

paste("El valor de la accion en ",t_anual,"años, estará dentro de ",precio_min_exp," y de", precio_max_exp," con un ",confianza*100,"% ")

########## Clase Dario 03-06-23 #########
rm(list=ls())
Z0 <- 10
dt <- 1/365
eps <- rnorm(1)
Z1 <- Z0 + eps * sqrt(dt)

eps <- rnorm(10000)
Z1 <- Z0 + eps * sqrt(dt)

mean (Z1)
sd(Z1)
sqrt(dt)

Z0 <- 10
dt <- 1/365
m <- 1000 # Cantidad de escenarios
Z <- matrix(NA,ncol=365,nrow=m)

Z[,1] = Z0

for(i in 1:m){
  for (j in 2:365) {
    Z[i,j] <- Z[i,j-1] + rnorm(1)*sqrt(dt)
  }
}

# Media de cada valor en el tiempo
lines(1:365,colmeans(X),lwd=5,col = "black")

# Con la raiz cuadrada del tiempo el desvio estandar va aumentando
