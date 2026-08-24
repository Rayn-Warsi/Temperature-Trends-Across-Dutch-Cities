rm(list=ls())

ols_estimate <- function(x, y) {
  
  # remove missing values
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  n <- length(y)
  
  # Check if we have enough data points
  if (n < 3) {
    cat("ERROR: Not enough valid data points for regression\n")
    return(NULL)
  }
  
  X        <- cbind(1, x)
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat    <- X %*% beta_hat
  e        <- y - y_hat
  sigma2   <- sum(e^2) / (n - 2)
  var_beta <- sigma2 * solve(t(X) %*% X)
  se_beta  <- sqrt(var_beta[2,2])
  t_stat   <- beta_hat[2] / se_beta
  p_value  <- 2 * (1 - pt(abs(t_stat), df = n - 2))
  alpha    <- 0.05 #significance level
  t_crit   <- qt(1 - alpha/2, df = n - 2) #critical value
  
  Sxx      <- sum((x - mean(x))^2)
  se_fit   <- sqrt(sigma2 * (1/n + (x - mean(x))^2 / Sxx))
  
  list(beta   = beta_hat,
       se     = se_beta,
       t      = t_stat,
       p      = p_value,
       ci_beta= c(beta_hat[2] - t_crit * se_beta,
                  beta_hat[2] + t_crit * se_beta),
       yhat   = y_hat,
       upper  = y_hat + t_crit * se_fit,
       lower  = y_hat - t_crit * se_fit)
}

#general analysis function (makes it easier to apply everything to the monthly data later)
run_temperature_analysis <- function(data, dataset_name,
                                     year_col = "Date",
                                     split_year = 1960,
                                     start_year = 1907,
                                     end_year = 2024) {
  
  #check if all required columns exist
  required_cols <- c(year_col, "De.Bilt", "Eelde", "Maastricht")
  missing_cols <- setdiff(required_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    cat("\nERROR:", dataset_name, "\n")
    cat("Missing columns:", paste(missing_cols, collapse=", "), "\n")
    cat("Available columns:", paste(colnames(data), collapse=", "), \n")
    return(invisible(NULL))
  }
  
  year <- data[[year_col]]
  
  # For annual data: years are already correct (1907, 1908, etc.)
  # For monthly data: dates are YYYYMM format, so extract year
  if (max(year) > 10000) {
    # Monthly data format (YYYYMM)
    year_numeric <- floor(year / 100)
  } else {
    # Annual data format (YYYY)
    year_numeric <- year
  }
  
  locations <- list(
    "De Bilt"     = data$De.Bilt,
    "Eelde"       = data$Eelde,
    "Maastricht"  = data$Maastricht
  )
  
  early_idx <- year_numeric <= split_year
  late_idx  <- year_numeric > split_year
  
  early_label <- paste(start_year, "-", split_year)
  late_label  <- paste(split_year + 1, "-", end_year)
  
  cat("\n\n=================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Early period:", early_label, "\n")
  cat("Late period :", late_label, "\n")
  cat("Total observations:", nrow(data), "\n")
  cat("=================================\n")
  
  par(mfrow = c(1,3))
  
  for (loc_name in names(locations)) {
    
    y <- locations[[loc_name]]
    
    m1 <- ols_estimate(seq_along(y[early_idx]), y[early_idx])
    m2 <- ols_estimate(seq_along(y[late_idx]),  y[late_idx])
    
    # Check if regression was successful
    if (is.null(m1) || is.null(m2)) {
      cat("\nSkipping", loc_name, "due to insufficient data\n")
      next
    }
    
    cat("\n---------------------------------\n")
    cat(loc_name, "|", early_label, "\n")
    cat(sprintf("Slope: %.6f | SE: %.6f | t: %.4f | p: %s\n95%% CI: [%.6f, %.6f]\n",
                m1$beta[2], m1$se, m1$t, format.pval(m1$p),
                m1$ci_beta[1], m1$ci_beta[2]))
    
    cat("\n")
    cat(loc_name, "|", late_label, "\n")
    cat(sprintf("Slope: %.6f | SE: %.6f | t: %.4f | p: %s\n95%% CI: [%.6f, %.6f]\n",
                m2$beta[2], m2$se, m2$t, format.pval(m2$p),
                m2$ci_beta[1], m2$ci_beta[2]))
    
    plot(year, y, pch = 16, col = "grey40",
         xlab = "Year", ylab = "Temperature (°C)",
         main = paste(dataset_name, "-", loc_name),
         cex.main = 1.1)
    
    lines(year[early_idx], m1$yhat,  col = "blue", lwd = 2)
    lines(year[early_idx], m1$upper, col = "blue", lty = 2)
    lines(year[early_idx], m1$lower, col = "blue", lty = 2)
    
    lines(year[late_idx],  m2$yhat,  col = "red",  lwd = 2)
    lines(year[late_idx],  m2$upper, col = "red",  lty = 2)
    lines(year[late_idx],  m2$lower, col = "red",  lty = 2)
    
    legend("topleft",
           legend = c("Observed",
                      paste("Trend", early_label),
                      paste("CI", early_label),
                      paste("Trend", late_label),
                      paste("CI", late_label)),
           col = c("grey40","blue","blue","red","red"),
           pch = c(16,NA,NA,NA,NA),
           lty = c(NA,1,2,1,2),
           lwd = c(NA,2,1,2,1),
           bty = "n", cex = 0.7)
  }
  
  par(mfrow = c(1,1))
}

#load data
annual_path <- "C:/Users/raynw/OneDrive/Desktop/MS/AnnualTemp.csv"
monthly_path <- "C:/Users/raynw/OneDrive/Desktop/MS/SmoothedMonthlyTemp.csv"

#load Annual Data
data_annual <- read.csv2(annual_path, sep=";", dec=",")
colnames(data_annual) <- c("Date", "De.Bilt", "Eelde", "Maastricht")
    
#load Monthly Data
data_monthly <- read.csv2(monthly_path, sep=";", dec=",")
colnames(data_monthly) <- c("Date", "De.Bilt", "Eelde", "Maastricht")

#run analysis
run_temperature_analysis(data_annual,  "Annual Data", 
                         split_year = 1960,
                         end_year = 2024)

run_temperature_analysis(data_monthly, "Smoothed Monthly Data",
                         split_year = 1960,
                         end_year = 2024)