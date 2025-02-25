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

x <- read_csv("05a_discharge/05a_discharge.csv", skip = 32)
print(paste0("Site #: ", x$site[1]))
y <- x %>%
     mutate(gage = 0.3048 * gage_ft, Q = 0.0929 * as.numeric(discharge_ft_per_s)) %>%
     select(-agency, -site, -gage_ft, -gage_qc, -discharge_ft_per_s, -discharge_qc) %>%
     mutate(dt = with_tz(mdy_hms(paste0(date, "T", time)), tz = "US/Eastern")) %>%
     select(dt, gage, Q)

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




ggplot(z) +
     geom_point(aes(x = Q, y = gage)) +
     xlab(TeX('Discharge $(m^3/s)$')) +
     ylab("Stage (m)") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))




