# ---- Ejercicio 2 - Primer Parcial - Juan Daumas - 900496

rm(list=ls())
library(tidyverse)
library(ggplot2)

#a)


VN = 100
coupon_rate = 0.05
Plazo = seq(1,12, by= 1)
TNA = c(0.0405, 0.0450, 0.0475, 0.0500, 0.0525, 0.0550, 0.0575, 0.0600, 0.0625, 0.0650, 0.0675, 0.0700)
TEA = TNA * Plazo/12
FD = (1+TEA)^-1

df <- tibble(t=seq(2,12,by=2),
             TEA = c(TEA[2],TEA[4],TEA[6],TEA[8],TEA[10],TEA[12]),
             FD_bi = (1+TEA)^-1,
             cupon = c(rep(VN * coupon_rate / 6, 5), VN * coupon_rate / 6 + VN),
             cash = cupon * FD_bi
)

price_bond = sum(df$cash)

cat("El precio del bono es $",price_bond)

# b)

#FRA 2x8

h = 2 #cuándo arranca
m = 8 #cuánto dura

fra_rate = ((1+TNA[h+m]*(h+m)/12)/(1+TNA[h]*(h)/12) - 1)*(12/m) #discreta
#fra_rate = (FD[h]/FD[h+m] - 1 )*(12/m)

libor = 0.0925
monto = 60000000
spread = 0.0105

FRA_payoff = (libor - fra_rate)*monto*(m/12)*(1 + libor*m/12)^(-1)

prestamo_monto = monto - FRA_payoff
prestamo_pago = prestamo_monto * (1+(libor+spread)*m/12)
CFT = ((prestamo_pago/monto) - 1)*12/m
cat("El CFT del préstamo es",CFT,", aproximadamente 8%.")

#c)

bim = seq(1,6)
TNA_bi = c(TNA[2],TNA[4],TNA[6],TNA[8],TNA[10],TNA[12])
FD_bim = (1+TNA_bi*bim/6)^(-1)

df_bim = tibble(bim,TNA_bi,FD_bim)

Swap_rate = ((1-FD_bim[6])/sum(FD_bim))*(12/2)

cat("La tasa del SWAP es",round(Swap_rate*100,4),"%")


