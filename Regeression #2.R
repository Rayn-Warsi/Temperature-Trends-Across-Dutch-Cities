rm(list=ls())

#regression function
run_whole_sample_regression <- function(data, dataset_name, year_col = "Date") {
  
  year <- data[[year_col]]
  
  #handle different date formats
  if (max(year) > 10000) {
    #monthly data format (YYYYMM): convert to decimal years for plotting
    year_plot <- year / 100
  } else {
    #annual data format (YYYY)
    year_plot <- year
  }
  
  locations <- list("De Bilt" = data$De.Bilt,
                    "Eelde" = data$Eelde,
                    "Maastricht" = data$Maastricht)
  
  n <- nrow(data)
  x <- 1:n
  X <- cbind(1, x)
  
  cat("\n\n=================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Observations:", n, "\n")
  cat("=================================\n")
  
  par(mfrow = c(1, 3))
  
  for (loc_name in names(locations)) {
    y <- locations[[loc_name]]
    
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
    y_hat    <- X %*% beta_hat
    e        <- y - y_hat
    sigma2   <- sum(e^2) / (n - 2)
    var_beta <- sigma2 * solve(t(X) %*% X)
    se_beta  <- sqrt(var_beta[2,2])
    t_stat   <- beta_hat[2] / se_beta
    p_value  <- 2 * (1 - pt(abs(t_stat), df = n - 2))
    
    Sxx    <- sum((x - mean(x))^2)
    t_crit <- qt(0.975, df = n - 2)
    se_fit <- sqrt(sigma2 * (1/n + (x - mean(x))^2 / Sxx))
    upper  <- y_hat + t_crit * se_fit
    lower  <- y_hat - t_crit * se_fit
    
    cat("\n---------------------------------\n")
    cat("Location:", loc_name, "\n")
    cat(sprintf("Intercept:   %.4f\n", beta_hat[1]))
    cat(sprintf("Slope:       %.4f\n", beta_hat[2]))
    cat(sprintf("SE:          %.4f\n", se_beta))
    cat(sprintf("t-statistic: %.4f\n", t_stat))
    cat(sprintf("p-value:     %s\n",   format.pval(p_value)))
    cat(sprintf("95%% CI:     [%.4f, %.4f]\n",
                beta_hat[2] - t_crit * se_beta,
                beta_hat[2] + t_crit * se_beta))
    
    plot(year_plot, y, pch = 16, col = "grey40",
         xlab = "Year", ylab = "Temperature (°C)",
         main = paste(dataset_name, "-", loc_name))
    lines(year_plot, y_hat, col = "red",  lwd = 2)
    lines(year_plot, upper, col = "blue", lty = 2)
    lines(year_plot, lower, col = "blue", lty = 2)
    legend("topleft",
           legend = c("Observed", "Trend", "95% CI"),
           col = c("grey40", "red", "blue"),
           pch = c(16, NA, NA), lty = c(NA, 1, 2),
           lwd = c(NA, 2, 1), bty = "n")
  }
  
  par(mfrow = c(1, 1))
}

#load data
annual_path <- "C:/Users/raynw/OneDrive/Desktop/MS/AnnualTemp.csv"
monthly_path <- "C:/Users/raynw/OneDrive/Desktop/MS/SmoothedMonthlyTemp.csv"

#load annual data
data_annual <- read.csv2(annual_path, sep=";", dec=",")
colnames(data_annual) <- c("Date", "De.Bilt", "Eelde", "Maastricht")

#load monthly data
data_monthly <- read.csv2(monthly_path, sep=";", dec=",")
colnames(data_monthly) <- c("Date", "De.Bilt", "Eelde", "Maastricht")

#run regression
run_whole_sample_regression(data_annual, "Annual Data")
run_whole_sample_regression(data_monthly, "Smoothed Monthly Data")