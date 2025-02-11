# Script to find the saturation water vapor
# Be sure to adjust temperature and relative humidity

T <- 273.15 + 20 # T is the temperature in K, this line converts from Celcius

a0 <- 6984.505294
a1 <- -188.9039310
a2 <- 2.133357675
a3 <- -1.288580973e-2
a4 <- 4.393587233e-5
a5 <- -8.023923082e-8
a6 <- 6.136820929e-11

e_star <- a0 + T * (a1 + T * (a2 + T * (a3 + T * (a4 + T * (a5 + T * a6 ) ) ) ) ) # hPa, of mb

r <- 0.45 # relative humidity
e_a <- r * e_star

a <- 1.24
b <- 1/7

eps_a <- a * (e_a / T)^b
