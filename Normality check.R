rm(list=ls())

# Load libraries
library(dplyr)
library(tidyr)

# Load data
data <- read.csv2("C:/Users/raynw/OneDrive/Desktop/MS/AnnualTemp.csv")
colnames(data) <- c("Year", "De_Bilt", "Eelde", "Maastricht")

# Convert to long format
data_long <- data %>%
  pivot_longer(cols = c("De_Bilt", "Eelde", "Maastricht"),
               names_to = "Location",
               values_to = "Temp")

# Create period split
data_long <- data_long %>%
  mutate(Period = ifelse(Year <= 1960, "1907-1960", "1961-2024"))

locations <- c("De_Bilt", "Eelde", "Maastricht")
periods   <- c("1907-1960", "1961-2024")

# Histograms
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

for (loc in locations) {
  for (per in periods) {
    
    temps <- data_long %>%
      filter(Location == loc, Period == per) %>%
      pull(Temp)
    
    hist(temps,
         freq   = FALSE,
         col    = "lightblue",
         border = "white",
         main   = paste(gsub("_", " ", loc), "|", per),
         xlab   = "Temperature (°C)",
         ylab   = "Density",
         ylim   = c(0, max(dnorm(mean(temps), mean(temps), sd(temps))) * 1.3),
         las    = 1)
    
    # Fitted normal using mean (red solid)
    curve(dnorm(x, mean = mean(temps), sd = sd(temps)),
          col = "red", lwd = 2, add = TRUE)
    
    # Fitted normal using median (green dashed) — highlights skew
    curve(dnorm(x, mean = median(temps), sd = sd(temps)),
          col = "darkgreen", lwd = 2, lty = 2, add = TRUE)
    
    legend("topright",
           legend = c("Normal (mean)", "Normal (median)"),
           col    = c("red", "darkgreen"),
           lwd    = 2, lty = c(1, 2),
           cex    = 0.7, bty = "n")
  }
}

# Shapiro-Wilk
cat("=== Shapiro-Wilk Normality Tests ===\n")
cat("(p > 0.05 suggests normality cannot be rejected)\n\n")

for (loc in locations) {
  for (per in periods) {
    
    temps <- data_long %>%
      filter(Location == loc, Period == per) %>%
      pull(Temp)
    
    sw <- shapiro.test(temps)
    
    cat(gsub("_", " ", loc), "|", per, "\n")
    cat("  Shapiro-Wilk: W =", round(sw$statistic, 4),
        " | p-value =", round(sw$p.value, 4), "\n\n")
  }
}
