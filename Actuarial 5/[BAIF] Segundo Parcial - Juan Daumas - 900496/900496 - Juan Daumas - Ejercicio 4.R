# 900496 - Juan Daumas - Segundo Parcial BAIF

#Ejercicio 4 - GAPs y RTI ----------------------------------------------------------

library(tidyverse)

rm(list=ls())

t = 0:6
sigma=0.03

Balance = tibble(t,
                 A_TV_c = c(300,300,300,300,300,300,0),
                 A_TV_am = c(0,0,0,0,0,0,300),
                 A_TF_c = c(700,700,700,700,700,700,0),
                 A_TF_am = c(0,0,0,0,0,0,700),
                 P_TV_c = c(100,100,100,100,0,0,0),
                 P_TV_am = c(0,0,0,0,100,0,0),
                 P_TF_c = c(800,800,800,800,0,0,0),
                 P_TF_am = c(0,0,0,0,800,0,0),
                 K = rep(100,7),
                 GAP_t = ifelse(t==0,0,A_TV_c + A_TF_c - P_TV_c - P_TF_c - K),
                 GAP_mg = ifelse(t==0,0,GAP_t - lag(GAP_t)))

#GAP_TV = Balance$A_TV_c[1]-Balance$P_TV_c[1]
#EaR = GAP_TV*qnorm(0.99,0,1)*sigma
# Calcular el eVaR de forma aproximada
# eVaR = abs(di*DURGAP)

# a) -------------------------------------------------------------------------------------
tf_a = 0.12

tf_p = 0.10

tv_a = 0.12

tv_p = 0.10

FF = tibble(t,
            ATV_am = Balance$A_TV_am,
            ATV_int = ifelse(t==0,0,lag(Balance$A_TV_c*(tv_a))),
            ATV_FD = (1+tv_a)^-t,
            ATF_am = Balance$A_TF_am,
            ATF_int = ifelse(t == 0,0, lag(Balance$A_TF_c*(tf_a))),
            ATF_FD = (1+tf_a)^-t,
            PTV_am = Balance$P_TV_am,
            PTV_int = ifelse(t==0,0,lag(Balance$P_TV_c*(tv_p))),
            PTV_FD = (1+tv_p)^-t,
            PTF_am = Balance$P_TF_am,
            PTF_int = ifelse(t == 0,0, lag(Balance$P_TF_c*(tf_p))),
            PTF_FD = (1+tf_p)^-t)

Atv = sum((FF$ATV_am+FF$ATV_int)*FF$ATV_FD)
Atf = sum((FF$ATF_am+FF$ATF_int)*FF$ATF_FD)
Ptv = sum((FF$PTV_am+FF$PTV_int)*FF$PTV_FD)
Ptf = sum((FF$PTF_am+FF$PTF_int)*FF$PTF_FD)

dur_ATF = sum(t*(FF$ATF_am+FF$ATF_int)*FF$ATF_FD)/Atf
dur_PTF = sum(t*(FF$PTF_am+FF$PTF_int)*FF$PTF_FD)/Ptf

DUR_ATF = dur_ATF/(1+tf_a)
DUR_PTF = dur_PTF/(1+tf_p)

DUR_ATV = 1/(1+tv_a)

DUR_PTV = 1/(1+tv_p)

DUR_GAP = -DUR_ATV*Atv - DUR_ATF*Atf + DUR_PTV*Ptv + DUR_PTF*Ptf

#b) ------- --------------------
cat("Como el Duration GAP es negativo, el peor escenario es una suba de tasas de interés. Los activos de la empresa, en cambio, subirían su valor económico si bajan las tasas de interés.")

#c) ------- Escenario nuevo --------------- subida de tasas de interés en 100 bps ---------------

#tf_a = 0

#tf_p = 0.20022

tv_a_n = 0.13

tv_p_n = 0.11

FF_n = FF %>%
  mutate(
    ATV_int = if_else(t == 0, 0, if_else(t == 1, lag(Balance$A_TV_c * tv_a), lag(Balance$A_TV_c * tv_a_n))),
    ATV_FD = (1+tv_a_n)^-t,
    ATF_FD = (1+tv_a_n)^-t,
    PTV_int = if_else(t == 0, 0, if_else(t == 1, lag(Balance$P_TV_c * tv_p), lag(Balance$P_TV_c * tv_p_n))),
    PTV_FD = (1+tv_p_n)^-t,
    PTF_FD = (1+tv_p_n)^-t,
    tot_A = (ATV_am+ATV_int+ATF_am+ATF_int),
    tot_P = (PTV_am+PTV_int+PTF_am+PTF_int)
  )

va_a = sum(FF_n$tot_A*(1+tv_a_n)^-t)
va_p = sum(FF_n$tot_P*(1+tv_p_n)^-t)

VA_c = Balance$K[1]

VA_c_n = va_a - va_p 

cat("Una baja de 100bps en las tasas de interés de activos y pasivos tiene un impacto negativo en el VE
    de",100 - VA_c_n/VA_c*100,"%, o de $",VA_c - VA_c_n,"pesos.")
