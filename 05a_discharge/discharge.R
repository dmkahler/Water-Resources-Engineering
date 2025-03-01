# This example uses data from Little Pine Creek, Etna, PA
# Access hydrology data: https://streamstats.usgs.gov/ss/
# Data download: https://waterdata.usgs.gov/nwis/uv?site_no=03049800
# Rating curve available: 05a_rating_curve.csv, skip = 35
# Discharge data: 05a_discharge.csv, skip = 32
# More streamgage information: https://www.usgs.gov/mission-areas/water-resources/science/streamgaging-basics

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(devtools)
install_github("LimpopoLab/hydrostats") # NEVER do this unless you know the origin of the code.
library(hydrostats)
library(e1071)

x <- read_csv("05a_discharge/05a_discharge.csv", skip = 32)
print(paste0("Site #: ", x$site[1]))
y <- x %>%
     mutate(gage = 0.3048 * gage_ft, Q = 0.0929 * as.numeric(discharge_ft_per_s)) %>%
     select(-agency, -site, -gage_ft, -gage_qc, -discharge_ft_per_s, -discharge_qc) %>%
     mutate(dt = with_tz(mdy_hms(paste0(date, "T", time)), tz = "US/Eastern")) %>%
     select(dt, gage, Q)

# plot one hydrologic year:
z <- y %>%
     filter(dt > ymd_hms("2022-09-30T00:00:00")) %>%
     filter(dt < ymd_hms("2023-10-01T00:00:00"))

ggplot(z) +
     geom_point(aes(x = dt, y = Q)) +
     xlab("Date") +
     ylab(TeX('Discharge $(m^3/s)$')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
# remember, this is 15-minute data; it should show just about everything.

# Return Period
# here, we will find the flood statistics for the river.
hy <- array(NA, dim = nrow(y))
for (i in 1:nrow(y)) {
     hy[i] <- hyd.yr(y$dt[i]) # must be in a loop, hyd.yr is not compatible with dplyr.
}
y <- data.frame(y,hy)
rm(hy)
z <- y %>%
     group_by(hy) %>%
     summarize(mn = mean(Q, na.rm = TRUE), mx = max(Q, na.rm = TRUE)) %>%
     mutate(LGmx = log(mx, base = 10))
m <- mean(z$LGmx)
s <- sd(z$LGmx)
# c <- skewness(z$LGmx) # from: e1071
c <- skew(z$LGmx) # from: hydrostats, there is a difference between methods.

## log-Pearson type III analysis
# For this distribution, we use the skewness to determine the 
# inverse CDF.
T_R <- 50 # return period in years (in this case)
Fx <- 1 - (1/T_R)
v <- pt3(c,Fx)
r <- (v * s) + m
flood <- 10^r # this is the T_R flood level

# Rating Curve
z <- y %>%
     filter(dt > ymd_hms("2023-09-30T00:00:00")) %>%
     filter(dt < ymd_hms("2024-10-01T00:00:00")) %>%
     filter(is.na(Q)==FALSE)
ggplot(z) +
     geom_point(aes(x = Q, y = gage)) +
     scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10') +
     xlab(TeX('Discharge $(m^3/s)$')) +
     ylab("Stage (m)") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))

# For a rating curve, simple regression lines don't cut it.
# We prepare the data for the Python script from USGS.
# Since data validation occurs every month, we will compare the data from September 2024:
zMod <- z %>%
     filter(dt >= ymd_hms("2024-09-01T00:00:00")) %>%
     filter(dt <= ymd_hms("2024-09-30T00:00:00")) %>%
     filter(gage < 0.9) %>%
     filter(Q < 0.9) %>%
     mutate(LOGgage = log(gage, base = 10)) %>%
     mutate(logQ = log(Q, base = 10))
ggplot(zMod) +
     geom_point(aes(x = Q, y = gage)) +
     scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10') +
     xlab(TeX('Discharge $(m^3/s)$')) +
     ylab("Stage (m)") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
write_csv(zMod, "05a_discharge/05a_sept2024.csv")



