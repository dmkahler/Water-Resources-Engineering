library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(latex2exp)

x <- read_csv("03_energy_example/2561295.csv") # This file is in the "metric" system per NOAA, precipitation and evaporation are in mm.

chick.pan <- x %>% 
      filter(STATION == "USC00341750") %>% 
      arrange(DATE) 

# Check data and convert to W/m^2
# For example, on 23 Aug 2008 at Chickashaw, there is a 95 mm evaporation, which is crazy.  This was after over a week of no observations.  This algorithm will remove any evaporation over 30 mm.
for (i in 1:(nrow(chick.pan))) {
      if ((chick.pan$EVAP[i] > 30) & (is.na(chick.pan$EVAP[i]) == FALSE)) {
            chick.pan$EVAP[i] <- NA
      }
      if ((is.na(chick.pan$TMIN[i]) == FALSE) & (is.na(chick.pan$TMAX[i]) == FALSE)) {
            t = (chick.pan$TMIN[i] + chick.pan$TMAX[i]) / 2
      } else {
            t <- 23 # Consider making it date dependent
      }
      Le <- -1.82948E-2*t^3 + 8.81543E-1*t^2 - 2.37904E+3*t + 2.50097e+6 # J/kg
      rho <- 1.641853602E-5*t^3 - 6.029365962E-3*t^2 + 2.617352384E-2*t + 9.999796842E+2 # kg/m^3
      # e (mm/d) / (1000 mm/m) / (86400 s/d) * (999 kg/m^3) * (2.265*10^6 J/kg) = W/m^2
      chick.pan$LeE[i] <- chick.pan$EVAP[i] * rho * Le / 86400000
}

ggplot(chick.pan, aes(x = DATE, y = EVAP)) +
      geom_line() +
      labs(x = "Date", y = "Evaporation (mm)") +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggplot(chick.pan, aes(x = DATE, y = LeE)) +
      geom_line() +
      labs(x = "Date", y = TeX('$Evaporation (W/m^{2})$')) +
      theme(panel.background = element_blank(), panel.border = element_rect(fill = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
