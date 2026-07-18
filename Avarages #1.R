rm(list=ls())

#rolling and block averages function
run_averages_analysis <- function(data, dataset_name, year_col = "Date") {
  
  year <- data[[year_col]]
  
  #handle different date formats
  if (max(year) > 10000) {
    #monthly data format (YYYYMM)
    year_plot <- year / 100
    window_size <- 120  # 10 years * 12 months
    block_size <- 120   # 10 years * 12 months
  } else {
    #annual data format (YYYY)
    year_plot <- year
    window_size <- 10   # 10 years
    block_size <- 10    # 10 years
  }
  
  bilt <- data$De.Bilt
  eelde <- data$Eelde
  maas <- data$Maastricht
  
  n <- length(year)
  
  cat("\n\n=================================\n")
  cat("Dataset:", dataset_name, "\n")
  cat("Observations:", n, "\n")
  cat("Rolling window:", window_size, "observations\n")
  cat("Block size:", block_size, "observations\n")
  cat("=================================\n")
  
  #rolling averages
  roll_bilt <- rep(NA, n)
  roll_eelde <- rep(NA, n)
  roll_maas <- rep(NA, n)
  
  for(i in window_size:n){
    roll_bilt[i] <- mean(bilt[(i-window_size+1):i])
    roll_eelde[i] <- mean(eelde[(i-window_size+1):i])
    roll_maas[i] <- mean(maas[(i-window_size+1):i])
  }
  
  #plotting rolling averages
  plot(year_plot, bilt,
       type="l",
       col="grey70",
       ylim=range(c(bilt, eelde, maas)),
       xlab="Year",
       ylab="Temperature (°C)",
       main=paste(dataset_name, "- Rolling Averages"))
  
  lines(year_plot, eelde, col="grey50")
  lines(year_plot, maas, col="grey30")
  
  lines(year_plot, roll_bilt, col="blue", lwd=2)
  lines(year_plot, roll_eelde, col="red", lwd=2)
  lines(year_plot, roll_maas, col="darkgreen", lwd=2)
  
  legend("topleft",
         legend=c("De Bilt (raw)", "Eelde (raw)", "Maastricht (raw)",
                  sprintf("De Bilt %.2fy avg", window_size/12),
                  sprintf("Eelde %.2fy avg", window_size/12),
                  sprintf("Maastricht %.2fy avg", window_size/12)),
         col=c("grey70","grey50","grey30","blue","red","darkgreen"),
         lty=1,
         lwd=c(1,1,1,2,2,2),
         cex=0.8)
  
  #block averages
  num_blocks <- floor(n / block_size)
  
  block_year <- rep(NA, num_blocks)
  block_bilt <- rep(NA, num_blocks)
  block_eelde <- rep(NA, num_blocks)
  block_maas <- rep(NA, num_blocks)
  
  for(i in 1:num_blocks){
    start <- (i-1)*block_size + 1
    end <- i*block_size
    
    block_year[i] <- mean(year_plot[start:end])
    block_bilt[i] <- mean(bilt[start:end])
    block_eelde[i] <- mean(eelde[start:end])
    block_maas[i] <- mean(maas[start:end])
  }
  
  cat("\nNumber of blocks:", num_blocks, "\n")
  
  #plotting block averages
  plot(block_year, block_bilt,
       type="b",
       pch=16,
       col="blue",
       ylim=range(c(block_bilt, block_eelde, block_maas)),
       xlab="Year",
       ylab="Average Temperature (°C)",
       main=paste(dataset_name, "- Block Averages"))
  
  lines(block_year, block_eelde, type="b", pch=16, col="red")
  lines(block_year, block_maas, type="b", pch=16, col="darkgreen")
  
  legend("topleft",
         legend=c("De Bilt", "Eelde", "Maastricht"),
         col=c("blue", "red", "darkgreen"),
         lty=1,
         pch=16)
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

#run averages analysis
par(mfrow = c(1, 2))
run_averages_analysis(data_annual, "Annual Data")

par(mfrow = c(1, 2))
run_averages_analysis(data_monthly, "Smoothed Monthly Data")

par(mfrow = c(1, 1))