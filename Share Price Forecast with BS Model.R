library(timeSeries)
library(quantmod)
library(magrittr)

# Download stock price & Use adjusted close price
getSymbols("^TWII", src = "yahoo")
Y <- matrix(TWII[, "TWII.Adjusted"])

# Forecasting periods & Historical time index
t <- 1:20
hist_end <- nrow(Y) - max(t)
hist_start <- hist_end - 120 + 1
Y.hist <- Y[hist_start:hist_end]

# Standard deviation based on historical compound returns
sv_hist <- Y.hist %>% returns(method = "continuous") %>% sd(na.rm = T)

# Mean historical return (drift term)
mu_hist <- Y.hist %>% returns(method = "continuous") %>% mean(na.rm = T)

# 95% CI for ln(St)
fc <- log(last(Y.hist)) + (mu_hist - 0.5*sv_hist^2)*t
fc_band <- list(
  m = exp(fc), 
  lb = exp(fc - qnorm(0.975)*sv_hist*sqrt(t)), 
  ub = exp(fc + qnorm(0.975)*sv_hist*sqrt(t))
  )

# absolute future time horizon
xt <- c(length(Y.hist) + t)

# stock price history line plot
plot(Y[hist_start:(hist_end + max(t))], 
     type = 'l',
     xlim = c(0, (hist_end - hist_start + 1) + max(t)),
     ylim = c(min(Y[hist_start:(hist_end + max(t))])*0.95, 
              max(Y[hist_start:(hist_end + max(t))])*1.05),
     xlab = "Time Index",
     ylab = "Stock Price",
     panel.first = grid())
# Mark starting time of the forecast
points(x = length(Y.hist), y = last(Y.hist), pch = 21, bg = "green")
# Line out CI and EV
for(i in fc_band){lines(x = xt, y = i, lty = 'dotted', col = "blue", lwd = 2)}
