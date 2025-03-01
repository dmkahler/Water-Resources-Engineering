# To compare output from USGS python script and USGS published rating curve for Little Pine Creek

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

x <- read_csv("05a_discharge/05a_discharge.csv", skip = 32)
print(paste0("Site #: ", x$site[1]))
x <- x %>%
     mutate(gage = 0.3048 * gage_ft, Q = 0.0929 * as.numeric(discharge_ft_per_s)) %>%
     select(-agency, -site, -gage_ft, -gage_qc, -discharge_ft_per_s, -discharge_qc) %>%
     mutate(dt = with_tz(mdy_hms(paste0(date, "T", time)), tz = "US/Eastern")) %>%
     select(dt, gage, Q) %>%
     mutate(type = "Historical")

dataIN <- x %>%
     filter(dt >= ymd_hms("2024-09-01T00:00:00")) %>%
     filter(dt <= ymd_hms("2024-09-30T00:00:00")) %>%
     select(gage, Q) %>%
     mutate(type = "Input Data")

x <- select(x, c(gage, Q, type))

y <- read_csv("05a_discharge/05a_rating_curve.csv", skip = 35)
y <- y %>%
     mutate(gage = 0.3048 * Stage_ft, Q = 0.0929 * as.numeric(Discharge_ft3_s)) %>%
     select(gage, Q) %>%
     mutate(type = "USGS Rating Curve")

z <- read_csv("05a_discharge/05a_rating_py.csv")
z <- z %>%
     rename(gage = stage, Q = discharge) %>%
     select(gage, Q) %>%
     mutate(type = "Calculated Rating Curve")

all <- rbind(x, dataIN, y, z)

ggplot(all) +
     geom_point(aes(x = Q, y = gage, color = type))

ggplot(all) +
     geom_point(aes(x = Q, y = gage, color = type)) +
     xlim(c(0,1)) +
     ylim(c(0,1))

