# Example 12.2.1 from Chow, Maidment, and Mays
# Annual maximum 10-minute-duration rainfall at Chicago, Il from 1913-1947
library(readr)
x <- read_csv("04_precip_frequency_analysis/precip_freq_example_data.csv")

# Use the Extreme Value Type I distribution to calculate the 5- 10- and 50-year return period maximum values for the 10-minute rainfall.
m <- mean(x$Rainfall)
s <- sd(x$Rainfall, na.rm = TRUE)

a <- ((sqrt(6))*s)/(pi)
u <- m - (0.5772 * a)

Tr <- 50 # This is the return period: {5, 10, 50}

y <- -log(log(Tr/(Tr-1)))
b <- u + (a * y) # This gives the intensity
