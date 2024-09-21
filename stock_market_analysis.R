### Time Series project script
# Package installation
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("fpp")

# Load packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(fpp)
library(forecast)
library(funggcast)
library(astsa)

# Load csv file, only care about date and close value
data <- read.csv("csv/AAPL.csv", sep = ",")[, c("Date", "Close")]
data$Date <- as.Date(data$Date, "%d-%m-%Y")
data <- data %>% filter(year(Date) >= 2010)

str(data)

# select either daily or weekly data
data_duration <- "weekly" # "daily" or "weekly"

if (data_duration == "weekly") {
      ## Group data by week
      stocks <- data %>%
                group_by(year = year(Date), week = week(Date) - 1) %>%
                summarize_if(is.numeric, mean)

      stocks$Date <- as.Date(paste(stocks$year,
                                   stocks$week, "1", sep = "-"),
                             "%Y-%U-%u")

      stocks <- stocks[, c("Date", "Close")]
      freq <- findfrequency(stocks)
      print(sprintf("Frequency: %s", freq))
      h <- 2 * freq
} else {
      stocks <- data[, c("Date", "Close")]
      freq <- findfrequency(stocks)
      print(sprintf("Frequency: %s", freq))
      h <- 4 * freq
}

# convert data to ts object and get training set
stocks_ts <- ts(stocks$Close, start = stocks$Date[1],
                frequency = freq)
str(stocks_ts)

# STATIONARITY AND OTHER THINGS
mean(stocks_ts)
var(stocks_ts)
decomp <- stl(stocks_ts, s.window = "per")
plot(decomp)

# check stationarity
adf.test(stocks_ts)
kpss.test(stocks_ts)

plot.ts(diff_stocks)
var(diff_stocks)
mean(diff_stocks)

adf.test(diff_stocks)
kpss.test(diff_stocks)

nsdiffs(stocks_ts) # not seasonal

train_ts <- ts(stocks_ts[seq(1, length(stocks_ts) - h)],
               frequency = freq)
str(train_ts)

test_ts <- stocks_ts[seq(length(stocks_ts) - h + 1, length(stocks_ts))]
str(test_ts)

# 1st difference
stocks$diff <- c(NA, diff(stocks_ts))

diff_plot <- ggplot(stocks, aes(x = Date)) +
             geom_line(aes(y = Close), colour = "#42b3d6") +
             geom_line(aes(y = diff), colour = "#0e0e62") +
             xlab("") + ylab("Value") +
             ggtitle("Weekly Apple stocks and 1st difference") +
             scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
             theme_pubr(base_size = 20)
diff_plot

# ACF and PACF of data and 1st diff
stocks_acf <- acf(stocks_ts, plot = FALSE)
diff_acf <- acf(diff(stocks_ts), plot = FALSE)

stocks_pacf <- pacf(stocks_ts, plot = FALSE)
diff_pacf <- pacf(diff(stocks_ts), plot = FALSE)

# conf intervals
ci_upper <- 1.96 / sqrt(length(stocks_ts))
ci_lower <- -1.96 / sqrt(length(stocks_ts))

acf_plot <- ggplot(data.frame(lag = seq(1, length(stocks_acf$lag)),
                              acf = stocks_acf$acf), aes(x = lag, y = acf)) +
             geom_bar(stat = "identity", fill = "#38aadf", width = 0.4) +
             geom_hline(yintercept = c(ci_upper, ci_lower),
                        linetype = "dashed", colour = "#b80843") +
             geom_hline(yintercept = 0, colour = "black") +
             xlab("Lag") + ylab("ACF") +
             ggtitle("ACF of the time series with daily data") +
             theme_pubr(base_size = 20)
acf_plot

pacf_plot <- ggplot(data.frame(lag = seq(1, length(stocks_pacf$lag)),
                              acf = stocks_pacf$acf), aes(x = lag, y = acf)) +
             geom_bar(stat = "identity", fill = "#38aadf", width = 0.4) +
             geom_hline(yintercept = c(ci_upper, ci_lower),
                        linetype = "dashed", colour = "#b80843") +
             geom_hline(yintercept = 0, colour = "black") +
             xlab("Lag") + ylab("PACF") +
             ggtitle("PACF of the time series with weekly data") +
             theme_pubr(base_size = 20)
pacf_plot

difF_acf_plot <- ggplot(data.frame(lag = seq(1, length(stocks_acf$lag)),
                                   acf = diff_acf$acf), aes(x = lag, y = acf)) +
             geom_bar(stat = "identity", fill = "#38aadf", width = 0.4) +
             geom_hline(yintercept = c(ci_upper, ci_lower),
                        linetype = "dashed", colour = "#b80843") +
             geom_hline(yintercept = 0, colour = "black") +
             xlab("Lag") + ylab("ACF") +
             ggtitle("ACF of the 1st difference of the time series") +
             theme_pubr(base_size = 20)
difF_acf_plot

difF_pacf_plot <- ggplot(data.frame(lag = seq(1, length(stocks_pacf$lag)),
                              acf = diff_pacf$acf), aes(x = lag, y = acf)) +
             geom_bar(stat = "identity", fill = "#38aadf", width = 0.4) +
             geom_hline(yintercept = c(ci_upper, ci_lower),
                        linetype = "dashed", colour = "#b80843") +
             geom_hline(yintercept = 0, colour = "black") +
             xlab("Lag") + ylab("PACF") +
             ggtitle("PACF of the 1st difference of the time series") +
             theme_pubr(base_size = 20)
difF_pacf_plot

### ETS models
# Additive
add_hw <- hw(train_ts, h = h, seasonal = "additive")
add_damp_hw <- hw(train_ts, h = h, seasonal = "additive", damped = TRUE)

# Multiplicative
mult_hw <- hw(train_ts, h = h, seasonal = "multiplicative")
mult_damp_hw <- hw(train_ts, h = h, seasonal = "multiplicative", damped = TRUE)
mult_exp_hw <- hw(train_ts, h = h, seasonal = "multiplicative",
                  exponential = TRUE)
mult_exp_damp_hw <- hw(train_ts, h = h, seasonal = "multiplicative",
                       exponential = TRUE, damped = TRUE)

# create ggplot from hw ets model
create_ggplot <- function(data, data_ts, h, model) {
      # add columns with model results
      data$fitted <- c(model$fitted, model$mean)
      data$color <- c(rep("red", times = length(data_ts) - h),
                        rep("black", times = h))
      data$lower <- c(rep(0, times = length(data_ts) - h),
                        model$lower[, 2])
      data$upper <- c(rep(0, times = length(data_ts) - h),
                        model$upper[, 2])

      # create ggplot object
      ts_plot <- ggplot(data, aes(x = Date, y = Close)) +
      geom_line(colour = "#12d42f") +
      geom_line(aes(y = fitted), colour = c(rep("red",
                times = length(data_ts) - h), rep("black", times = h))) +
      ggtitle(paste(sprintf("%s applied to Apple stocks,", model$model$method),
      sprintf("forecasting the last %i values", h), sep = " ")) +
      xlab("") + ylab("Value") +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
      coord_cartesian(xlim = as.Date(c("2020", "2022"), format = ("%Y"))) +
      theme_pubr(base_size = 15)

      return(ts_plot)
}

# create ggplot object of each model
add_ts <- create_ggplot(stocks, stocks_ts, h, add_hw)
add_damp_ts <- create_ggplot(stocks, stocks_ts, h, add_damp_hw)
mult_ts <- create_ggplot(stocks, stocks_ts, h, mult_hw)
mult_damp_ts <- create_ggplot(stocks, stocks_ts, h, mult_damp_hw)
mult_exp_ts <- create_ggplot(stocks, stocks_ts, h, mult_exp_hw)
mult_exp_damp_ts <- create_ggplot(stocks, stocks_ts, h, mult_exp_damp_hw)

# plot model and forecasts
ggarrange(add_ts, nrow = 1, ncol = 1)
ggarrange(add_damp_ts, nrow = 1, ncol = 1)
ggarrange(mult_ts, nrow = 1, ncol = 1)
ggarrange(mult_damp_ts, nrow = 1, ncol = 1)
ggarrange(mult_exp_ts, nrow = 1, ncol = 1)
ggarrange(mult_exp_damp_ts, nrow = 1, ncol = 1)

# model decomposition
plot(add_hw$model)
plot(add_damp_hw$model)
plot(mult_hw$model)
plot(mult_damp_hw$model)
plot(mult_exp_hw$model)
plot(mult_exp_damp_hw$model)

# Training and Test metrics of each model
accuracy(add_hw, test_ts)
accuracy(add_damp_hw, test_ts)
accuracy(mult_hw, test_ts)
accuracy(mult_damp_hw, test_ts)
accuracy(mult_exp_hw, test_ts)
accuracy(mult_exp_damp_hw, test_ts)

# SARIMA NEW
lambda <- BoxCox.lambda(train_ts)

ref_model <- auto.arima(train_ts, seasonal = TRUE, stationary = FALSE,
                        lambda = lambda)
ref_model


sarima_model <- Arima(train_ts, order = c(1, 1, 1), seasonal = c(2, 0, 0))
                  #     include.drift = TRUE)

resid <- sarima_model$residuals
mean(resid)
var(resid)

Box.test(resid, lag = 10, type = "Box-Pierce")
shapiro.test(resid)
ks.test(resid, pnorm, mean(resid), sqrt(var(resid)))


create_ggplot2 <- function(data, data_ts, h, model) {
      fc <- forecast(model, h = h)

      # add columns with model results
      data$fitted <- c(model$fitted, fc$mean)
      data$color <- c(rep("red", times = length(data_ts) - h),
                        rep("black", times = h))
      data$lower <- c(rep(0, times = length(data_ts) - h),
                        fc$lower[, 2])
      data$upper <- c(rep(0, times = length(data_ts) - h),
                        fc$upper[, 2])

      # create ggplot object
      ts_plot <- ggplot(data, aes(x = Date, y = Close)) +
      geom_line(colour = "#12d42f", show.legend = TRUE) +
      geom_line(aes(y = fitted), colour = c(rep("red",
                times = length(data_ts) - h), rep("black", times = h))) +
      xlab("") + ylab("Value") +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
      theme_pubr(base_size = 20)

      return(ts_plot)
}

ts_plot <- create_ggplot2(stocks, stocks_ts, h, sarima_model)

ts_plot + ggtitle(paste("SARIMA (1,1,1)x(2,0,0)_16 applied to Apple stocks,",
          sprintf("forecasting the last %i values", h), sep = " "))


accuracy(forecast(model, h = h), test_ts)
