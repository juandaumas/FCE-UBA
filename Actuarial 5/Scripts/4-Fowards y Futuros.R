## Forwards y Futuros
rm(list = ls())

# Activo sin rendimiento

Ft <- St * FC_t_T

# Activo con rendimiento
Ft <- (St - I) * FC_t_T

# Activo con costo de almacenamiento
Ft <- (St + U) * FC_t_T

# Activo con rendimiento porcentual asociado
Ft <- St * FC_t_T/FC2_t_T

# Forward sobre moneda
F0 <- S0 * exp((r-rf)*t)

# Valor Forward
ValorForward <- (Ft-K)*FD_t_T