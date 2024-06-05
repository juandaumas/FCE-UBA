######### OPCIONES ##########
rm(list = ls())

library(ggplot2)
# Opcion de compra - CAll - PayOff lado comprador
ST <- 55
K <- 30
Prima <- 3
max(ST-K,0)

ST_Vector <- c(seq(20,50,by = 1))
Pay_off_Call <- NULL
for (i in 1:length(ST_Vector)){
  Pay_off_Call[i] <- max(ST_Vector[i] - K, 0)
}
Pay_off_Call - 3 

grafico_call_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y=Pay_off_Call, color="blue")) +
  geom_line(aes(x=ST_Vector, y=Pay_off_Call-3, color="grey")) +
  geom_abline(slope = 0, linetype = "dashed", color = "black") +
  labs(title = "Pay Off de un Call", subtitle = "Prima 3", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("blue", "grey"), labels=c("Pay Off sin prima", "Pay Off con prima")) +
  theme_bw()

grafico_call_payoff

# Opcion de compra - CAll - PayOff lado vendedor
ST <- 55
K <- 30
Prima <- 3
max(ST-K,0)

ST_Vector <- c(seq(20,50,by = 1))
Pay_off_Call <- NULL
for (i in 1:length(ST_Vector)){
  Pay_off_Call[i] <- -max(ST_Vector[i] - K, 0)
}
Pay_off_Call - 3 

grafico_call_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y=Pay_off_Call, color="blue")) +
  geom_line(aes(x=ST_Vector, y=Pay_off_Call+3, color="grey")) +
  geom_abline(slope = 0, color = "black", linetype = "dashed") +
  labs(title = "Pay Off de un Call", subtitle = "Prima 3", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("blue", "grey"), labels=c("Pay Off sin prima", "Pay Off con prima")) +
  theme_bw()

grafico_call_payoff

# Opcion de compra - PUT - PayOff lado comprador

ST <- 55
K <- 30
Prima <- 3
max(ST-K,0)

ST_Vector <- c(seq(20,50,by = 1))
Pay_off_put <- NULL
for (i in 1:length(ST_Vector)){
  Pay_off_put[i] <- max(K -ST_Vector[i], 0)
}
Pay_off_put - 4

grafico_put_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y=Pay_off_put, color="blue")) +
  geom_line(aes(x=ST_Vector, y=Pay_off_put-4, color="grey")) +
  labs(title = "Pay Off de un Put", subtitle = "Prima 3", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("blue", "grey"), labels=c("Pay Off sin prima", "Pay Off con prima")) +
  theme_bw()

grafico_put_payoff

# Opcion de compra - PUT - PayOff lado vendedor

ST <- 55
K <- 30
Prima <- 3
max(ST-K,0)

ST_Vector <- c(seq(20,50,by = 1))
Pay_off_put <- NULL
for (i in 1:length(ST_Vector)){
  Pay_off_put[i] <- -max(K -ST_Vector[i], 0)
}
Pay_off_put - 4

grafico_put_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y=Pay_off_put, color="blue")) +
  geom_line(aes(x=ST_Vector, y=Pay_off_put+4, color="grey")) +
  labs(title = "Pay Off de un Put", subtitle = "Prima 3", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("blue", "grey"), labels=c("Pay Off sin prima", "Pay Off con prima")) +
  theme_bw()

grafico_put_payoff

# 1 - Opciones + Subyacente
## Short Call + Long subyacente

rm(list = ls())

ST <- 10
S0 <- 11
K <- 10
prima <- 3

ST_Vector <- c(seq(0,25,by = 1))
Pay_off_cartera <- NULL
Pay_off_short_call <- NULL
Pay_off_subyacente <- NULL

for (i in 1:length(ST_Vector)){
  Pay_off_short_call[i] <- -max(ST_Vector[i] - K, 0)
  Pay_off_subyacente[i] <- ST_Vector[i] - S0
  Pay_off_cartera[i] <- Pay_off_subyacente[i] + Pay_off_short_call[i]
}


grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_cartera+prima, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= Pay_off_subyacente,color = "Long Subyacente"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= Pay_off_short_call+prima,color = "Pay Off Short Call"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Short Call + Long Subyacente", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Long Subyacente" = "grey", "Pay Off Short Call" = "blue"), 
                     labels=c("Estrategia", "Long Subyacente", "Pay Off Short Call"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 1 - Opciones + Subyacente
## Short PUT + SHORT subyacente

rm(list = ls())

ST <- 11
S0 <- 9
K <- 10
prima <- 3

ST_Vector <- c(seq(0,25,by = 1))
Pay_off_cartera <- NULL
Pay_off_short_put <- NULL
Pay_off_subyacente <- NULL

for (i in 1:length(ST_Vector)){
  Pay_off_short_put[i] <- -max(K - ST_Vector[i], 0)
  Pay_off_subyacente[i] <- S0 - ST_Vector[i]
  Pay_off_cartera[i] <- Pay_off_subyacente[i] + Pay_off_short_put[i]
}


grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_cartera, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= Pay_off_subyacente,color = "Short Subyacente"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= Pay_off_short_put,color = "Pay Off Short Put"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Short Put + Short Subyacente", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Short Subyacente" = "grey", "Pay Off Short Put" = "blue"), 
                     labels=c("Estrategia","Pay Off Short Put", "Short Subyacente"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 1 - Opciones + Subyacente
## LONG CALL + SHORT Subyacente

rm(list = ls())

ST <- 10
S0 <- 11
K <- 10
prima <- 2

ST_Vector <- c(seq(0,25,by = 1))
Pay_off_cartera <- NULL
Pay_off_long_call <- NULL
Pay_off_subyacente <- NULL

for (i in 1:length(ST_Vector)){
  Pay_off_long_call[i] <- max(ST_Vector[i] - K, 0)
  Pay_off_subyacente[i] <- S0 - ST_Vector[i]
  Pay_off_cartera[i] <- Pay_off_subyacente[i] + Pay_off_long_call[i]
}


grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_cartera-prima, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= Pay_off_subyacente,color = "Short Subyacente"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= Pay_off_long_call,color = "Pay Off Long Call"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Long Call + Short Subyacente", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Short Subyacente" = "grey", "Pay Off Long Call" = "blue"), 
                     labels=c("Estrategia", "Pay Off Long Call","Short Subyacente"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 1 - Opciones + Subyacente
## LONG Put + Long Subyacente

rm(list = ls())

ST <- 10
S0 <- 10
K <- 10
prima <- 1

ST_Vector <- c(seq(0,25,by = 1))
Pay_off_cartera <- NULL
Pay_off_long_put <- NULL
Pay_off_subyacente <- NULL

for (i in 1:length(ST_Vector)){
  Pay_off_long_put[i] <- max(K - ST_Vector[i], 0)
  Pay_off_subyacente[i] <- ST_Vector[i] - S0
  Pay_off_cartera[i] <- Pay_off_subyacente[i] + Pay_off_long_put[i]
}

Pay_off_long_put_2 <- pmax(K-ST_Vector,0)

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_cartera-prima, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= Pay_off_subyacente,color = "Long Subyacente"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= Pay_off_long_put,color = "Pay Off Long Put"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Long Call + Long Subyacente", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Long Subyacente" = "grey", "Pay Off Long Put" = "blue"), 
                     labels=c("Estrategia", "Long Subyacente", "Pay Off Long Put"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 2.1 Bull Spreads: con CALLS

rm(list = ls())

K1 <- 10 # K1 es Long (ST-K)
K2 <- 15 # K2 es Short -(ST-K)
prima_K1 <- 3
prima_K2 <- 5

ST_Vector <- seq(0, 25, by = 1)


payoff_long <- pmax(ST_Vector - K1, 0) - prima_K1
payoff_short <- pmin(K2 - ST_Vector, 0) + prima_K2

Pay_off_estrategia <- payoff_long + payoff_short

grafico_cartera_payoff_1 <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Pay Off estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bull Spread de Calls", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Pay Off estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "grey"), 
                     labels=c("Pay Off estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff_1

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 2.1 Bull Spreads: con PUTS

rm(list = ls())

K1 <- 10 # K1 es Long (ST-K)
K2 <- 15 # K2 es Short -(ST-K)
prima_K1 <- 3
prima_K2 <- 5

ST_Vector <- seq(0, 30, by = 1)

payoff_long <- pmax(K1 - ST_Vector, 0) - prima_K1
payoff_short <- pmin(ST_Vector - K2, 0) + prima_K2

Pay_off_estrategia <- payoff_long + payoff_short

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Pay Off estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bull Spread de Calls", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Pay Off estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "grey"), 
                     labels=c("Pay Off estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 22 Bear Spreads: con PUTS

rm(list = ls())

K1 <- 10 # K1 es Short -(K-ST)
K2 <- 15 # K2 es Long (K-ST)
prima_K1 <- 3
prima_K2 <- 5

ST_Vector <- c(seq(5, 20, by = 1))

payoff_short <- -pmax(K1 - ST_Vector, 0) + prima_K1
payoff_long <- pmax(K2 - ST_Vector, 0) - prima_K2

Pay_off_estrategia <- payoff_short + payoff_long

grafico_cartera_payoff_2 <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "grey"), 
                     labels=c("Estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff_2

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 22 Bear Spreads: con CALLS

rm(list = ls())

K1 <- 10 # K1 es Short Call -(ST - K)
K2 <- 15 # K2 es Long Call (ST - K)
prima_K1 <- 3
prima_K2 <- 5

ST_Vector <- c(seq(0, 25, by = 1))

payoff_short <- -pmax(ST_Vector - K1, 0) + prima_K1
payoff_long <- pmax(ST_Vector - K2, 0) - prima_K2

Pay_off_estrategia <- payoff_short + payoff_long

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_short,color = "Pay Off Short"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long,color = "Pay Off Long"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Long" = "blue", "Pay Off Short" = "grey"), 
                     labels=c("Estrategia", "Pay Off Long", "Pay Off Short"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 2.3 Box Spreads: Bull Call Spread + Bear Put Spread

rm(list = ls())

K1 <- 10 # K1 es Long Call (ST - K)
K2 <- 15 # K2 es Short Call -(S-K)
K3 <- 10 # K3 es Short Put -(K-ST)
K4 <- 15 # K4 es Long Put (K-ST)

prima_K1 <- 2
prima_K2 <- 5
prima_K3 <- 2
prima_K4 <- 5

ST_Vector <- c(seq(0, 20, by = 1))

payoff_long_call <- pmax(ST_Vector - K1, 0) - prima_K1
payoff_short_call <- -pmax(ST_Vector - K2, 0) + prima_K2
payoff_long_put <- -pmax(K1 - ST_Vector, 0) - prima_K3
payoff_short_put <- pmax(K2 - ST_Vector, 0) + prima_K4

Pay_off_estrategia_bull <- payoff_long_call + payoff_short_call 
Pay_off_estrategia_bear <- payoff_long_put + payoff_short_put

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia_bull, color="Estrategia Bull"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia_bear, color="Estrategia Bear"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long_call, color="Pay Off Long call"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short_call,color = "Pay Off Short Call"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long_put,color = "Pay Off Long Put"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short_put,color = "Pay Off Short Put"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia Bull" = "red","Estrategia Bear" = "red","Pay Off Long call" = "blue", "Pay Off Short Call" = "grey","Pay Off Long Put" = "black","Pay Off Short Put" = "violet"), 
                     labels=c("Estrategia Bull","Estrategia Bear", "Pay Off Long call", "Pay Off Short Call","Pay Off Long Put","Pay Off Short Put"),
                     guide = guide_legend(override.aes = list(linetype = c("solid","solid", "dashed", "dashed","dashed","dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 22 Butterfly Spread 2 Long Call 1 Short Call

rm(list = ls())

K1 <- 10 # K1 es Long Call (ST - K)
K2 <- 15 # K2 es Short Call -(ST - K)
K3 <- 20 # K3 es Long Call (ST - K)
prima_K1 <- 5
prima_K2 <- 3
prima_K3 <- 4

ST_Vector <- c(seq(0, 25, by = 1))

payoff_long_call_1 <- pmax(ST_Vector - K1, 0) - prima_K1
payoff_long_call_2 <- pmax(ST_Vector - K3, 0) - prima_K3
payoff_short_call <- -pmax(ST_Vector - K2,0) + prima_K2

Pay_off_estrategia <- payoff_long_call_1 + payoff_long_call_2 + 2*payoff_short_call

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long_call_1,color = "Pay Off Long Call 1"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long_call_2,color = "Pay Off Long Call 2"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short_call,color = "Pay Off Short Call"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Long Call 1" = "blue","Pay Off Long Call 2" = "black", "Pay Off Short Call" = "grey"), 
                     labels=c("Estrategia", "Pay Off Long Call 1","Pay Off Long Call 2", "Pay Off Short Call"),
                     guide = guide_legend(override.aes = list(linetype = c("solid","dashed", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 2 - Spreads: dos o mas opciones de un mismo tipo
## 22 Butterfly Spread 2 Long Put Call 1 Short Put

rm(list = ls())

K1 <- 10 # K1 es Long Call (ST - K)
K2 <- 15 # K2 es Short Call -(ST - K)
K3 <- 20 # K3 es Long Call (ST - K)
prima_K1 <- 5
prima_K2 <- 3
prima_K3 <- 4

ST_Vector <- c(seq(0, 25, by = 1))

payoff_long_put_1 <- pmax(K1 - ST_Vector, 0) - prima_K1
payoff_long_put_2 <- pmax(K3 - ST_Vector, 0) - prima_K3
payoff_short_put <- -pmax(K2 - ST_Vector,0) + prima_K2

Pay_off_estrategia <- payoff_long_put_1 + payoff_long_put_2 + 2 * payoff_short_put

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_long_put_1,color = "Pay Off Long put 1"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_long_put_2,color = "Pay Off Long put 2"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_short_put,color = "Pay Off Short put"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Bear Spread de PUTS", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Long put 1" = "blue","Pay Off Long put 2" = "black", "Pay Off Short put" = "grey"), 
                     labels=c("Estrategia", "Pay Off Long put 1","Pay Off Long put 2", "Pay Off Short put"),
                     guide = guide_legend(override.aes = list(linetype = c("solid","dashed", "dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff

# 3 - Combinaciones de dos o mas opciones de distinto tipo
## 3.1 Stradlle: Long Call y Long Put 

K <- 10
Prima_Call <- 2
Prima_Put <- 3

ST_Vector <- seq(0,20,1)

payoff_call <- pmax(ST_Vector - K, 0) + Prima_Call
payoff_put <- pmax(K - ST_Vector, 0) + Prima_Put

Pay_off_estrategia <- payoff_call + payoff_put

grafico_cartera_payoff <- ggplot() + 
  geom_line(aes(x=ST_Vector, y= Pay_off_estrategia, color="Estrategia"), linetype = "solid") +
  geom_line(aes(x=ST_Vector, y= payoff_call,color = "Pay Off Call"), linetype = "dashed") +
  geom_line(aes(x=ST_Vector, y= payoff_put,color = "Pay Off Put"), linetype = "dashed") +
  labs(title = "Pay Off estrategia", subtitle = "Straddle", x = "Precio del subyacente", y = "Pay Off", color = "Leyenda") +
  scale_color_manual(values=c("Estrategia" = "red", "Pay Off Call" = "blue","Pay Off Put" = "black"), 
                     labels=c("Estrategia", "Pay Off Call","Pay Off Put"),
                     guide = guide_legend(override.aes = list(linetype = c("solid","dashed", "dashed")))) +
  theme_bw()

grafico_cartera_payoff




########################## OPCIONES ###################################
rm(list=ls())
###### Precios Limites - Superiores

#     c =< S0  &  C =< S0

#     p =< K  $   P =< K

###### Precios Limites - Inferiores

#   S0 - K*e^rt

S = 20
K = 18
r = 0.10
t = 1

call <- function(S,K,r,t){
  call <- S - K*exp(-r*t)
  return(call)
}
call(S,K,r,t)

call_mercado = 3

# Call sobre accion que no paga dividendo
arbitraje <- function(S,K,r,t,ST,valor_mercado,valor_teorico,tipo_derivado = 'Call',tipo_interes = "Continua"){
  if (tipo_interes == "Continua"){
    FC <- exp(r*t)
    FD <- exp(-r*t)
  } else if (tipo_interes == "Efectiva"){
    FC <- (1+r)^t
    FD <- (1+r)^-t
  } else {
    print("El tipo_interes tiene que ser: Continua o Efectiva")
    stop()
  }
  if(valor_mercado < valor_teorico){
    print("Compro Call - Vendo subyacente - Invierno a TLR")
    cash_flow <- (S - valor_mercado)*FC
  } else if (valor_mercado > valor_teorico) {
    print("Vendo Call - Compro subyacente - Tomo Deuda a TLR")
    cash_flow <- (valor_mercado-S)*FC
  }
  if (ST>K){
    print("Call se ejerce") 
    arbitraje <- cash_flow - K
  } else{
    print("Call no se ejerce") 
    arbitraje <- cash_flow - ST
  }
  arbitraje
}

arbitraje(S = 20, K= 18, ST = 17,
          r = 0.1,t = 1,
          valor_teorico = 3.71,
          valor_mercado = 3)

arbitraje <- function(S, K, r, t, ST, valor_mercado, tipo_derivado = 'Call', tipo_interes = "Continua") {
  if (tipo_derivado == "Call") {
    
    if (tipo_interes == "Continua") {
      FC <- exp(r*t)
      FD <- exp(-r*t)
    } else if (tipo_interes == "Efectiva") {
      FC <- (1+r)^t
      FD <- (1+r)^-t
    } else {
      cat("El tipo_interes tiene que ser: Continua o Efectiva\n")
      stop()
    }
    valor_teorico <- (S - K*FD)
    
    if (valor_mercado < valor_teorico) {
      cat("Compro Call - Vendo subyacente - Invierno a TLR\n")
      cash_flow <- (S - valor_mercado)*FC
    } else if (valor_mercado > valor_teorico) {
      cat("Vendo Call - Compro subyacente - Tomo Deuda a TLR\n")
      cash_flow <- (valor_mercado - S)*FC
    }
    
    if (ST > K) {
      cat("Call se ejerce\n")
      arbitraje <- cash_flow - K
    } else {
      cat("Call no se ejerce\n")
      arbitraje <- cash_flow - ST
    }
    return(valor_teorico)
    return(arbitraje)
    
  } else if (tipo_derivado == "Put") {
    if (tipo_interes == "Continua") {
      FC <- exp(r*t)
      FD <- exp(-r*t)
    } else if (tipo_interes == "Efectiva") {
      FC <- (1+r)^t
      FD <- (1+r)^-t
    } else {
      cat("El tipo_interes tiene que ser: Continua o Efectiva\n")
      stop()
    }
    valor_teorico <- (K*FD - S)
    
    if (valor_mercado < valor_teorico) {
      cat("Compro Put - Compro subyacente - Tomo deuda a TLR\n")
      cash_flow <- (valor_mercado + S)*FC
    } else if (valor_mercado > valor_teorico) {
      cat("Vendo Put - Vendo subyacente - Invierto a TLR\n")
      cash_flow <- (valor_mercado + S) * FC
    }
    
    if (ST > K) {
      cat("Put no se ejerce\n")
      arbitraje <- ST - cash_flow
    } else {
      cat("Put se ejerce\n")
      arbitraje <- K - cash_flow
    }
    return(valor_teorico)
    return(arbitraje)
  }
}
arbitraje(S = 37,K = 40,r = 0.05,t = 0.5,ST = 10,valor_mercado = 1,
        tipo_derivado = "Put",tipo_interes = "Continua")

arbitraje(S = 38,K = 40,r = 0.1,t = 0.25,ST = 10,valor_mercado = 1,
          tipo_derivado = "Put",tipo_interes = "Continua")
