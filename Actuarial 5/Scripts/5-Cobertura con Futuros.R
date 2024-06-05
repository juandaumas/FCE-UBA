# Cobertura con Futuros

rm(list = ls())

# Cobertura de cartera con Futuros

cant_futuros <- (beta_esp - beta_cartera)*valor_cartera/(precio_futuro * mult_indice)

# Cobertura de IRF

Hd <- ((Dur_mod_esp - Dur_portafolio)/ Dur_mod_suby ) *  Nominal * Beta_yield / Valor_Futuro

