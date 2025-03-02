# Rating Curve

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(devtools)
install_github("LimpopoLab/hydrostats") # NEVER do this unless you know the origin of the code.
library(hydrostats)

# Get data
x <- read_csv("05a_discharge/05a_discharge.csv", skip = 32)
print(paste0("Site #: ", x$site[1]))
y <- x %>%
     mutate(gage = 0.3048 * gage_ft, Q = 0.0929 * as.numeric(discharge_ft_per_s)) %>%
     select(-agency, -site, -gage_ft, -gage_qc, -discharge_ft_per_s, -discharge_qc) %>%
     mutate(dt = with_tz(mdy_hms(paste0(date, "T", time)), tz = "US/Eastern")) %>%
     select(dt, gage, Q)

# Add hydrologic year
hy <- array(NA, dim = nrow(y))
for (i in 1:nrow(y)) {
     hy[i] <- hyd.yr(y$dt[i]) # must be in a loop, hyd.yr is not compatible with dplyr.
}
y <- data.frame(y,hy)
rm(hy)

# Filter time
# For a rating curve, simple regression lines don't cut it.
# We prepare the data for the Python script from USGS.
# Since data validation occurs every month, we will compare the data from September 2024:
z <- y %>%
     filter(dt >= ymd_hms("2024-09-01T00:00:00")) %>%
     filter(dt <= ymd_hms("2024-09-30T00:00:00")) %>%
     filter(is.na(Q)==FALSE) %>%
     mutate(LOGgage = log(gage, base = 10)) %>%
     mutate(logQ = log(Q, base = 10))
ggplot(z) +
     geom_point(aes(x = Q, y = gage)) +
     scale_y_continuous(trans='log10') + scale_x_continuous(trans='log10') +
     xlab(TeX('Discharge $(m^3/s)$')) +
     ylab("Stage (m)") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14)) +
     theme(axis.title = element_text(face = "plain", size = 14))
write_csv(z, "05a_discharge/05a_sept2024.csv")



