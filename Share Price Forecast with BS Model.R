library(timeSeries)
library(quantmod)
library(magrittr)

# Packing Function
forecast.BS <- function(data, future.t=20, history.t=120){
  # Forecasting periods & Historical time index
  t <- 1:future.t
  hist_end <- nrow(data) - future.t
  hist_start <- hist_end - 120 + 1
  Y.hist <- data[hist_start:hist_end]
  
  # Standard deviation and mean of historical compound returns
  sv_hist <- Y.hist %>% returns(method = "continuous") %>% sd(na.rm = T)
  mu_hist <- Y.hist %>% returns(method = "continuous") %>% mean(na.rm = T)
  
  # 95% CI for ln(St)
  fc <- log(last(Y.hist)) + (mu_hist - 0.5*sv_hist^2)*t
  fc_band <- list(m = exp(fc), 
                  lb = exp(fc - qnorm(0.975)*sv_hist*sqrt(t)), 
                  ub = exp(fc + qnorm(0.975)*sv_hist*sqrt(t)))
  
  # stock price history line plot
  plot(data[hist_start:(hist_end + future.t)], type = 'l',
       xlim = c(0, (hist_end - hist_start + 1) + future.t),
       ylim = c(min(data[hist_start:(hist_end + future.t)])*0.95, 
                max(data[hist_start:(hist_end + future.t)])*1.05),
       xlab = "Time Index", ylab = "Stock Price",
       panel.first = grid())
  # Mark starting time of the forecast
  points(x = length(Y.hist), y = last(Y.hist), pch = 21, bg = "green")
  # Line out CI and EV
  for(i in fc_band){lines(x = c(length(Y.hist) + t), y = i, lty = 'dotted', col = "blue", lwd = 2)}
}

# Download stock price & Use adjusted close price
getSymbols("^TWII", src = "yahoo")
matrix(TWII[, "TWII.Adjusted"]) %>% forecast.BS