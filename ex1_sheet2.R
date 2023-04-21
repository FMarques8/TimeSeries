# ex2 of sheet 2
install.packages("astsa")
require(astsa)

gtemp
ts.plot(gtemp)

# model: x_t=mu_t+w_t

# a) modelling the trend using mu_t=b_1+b_2*t
time(gtemp)
fit <- lm(gtemp ~ time(gtemp), na.action = NULL)
fit

# Coefficients:
# (Intercept->b)  time(gtemp)->x
#  -11.199496     0.005748

# adjusted model: mu_t_est=-11.2+0.006t
# w_t_est=x_t+11.2-0.006t

# b) plot the time series without the trend
par(mfrow = c(2, 1))
ts.plot(resid(fit), type = "o", main = "detrended") #w_t_est
ts.plot(resid(fit), main = "detrended")

# c) other way of considering the stabilization in 
# mean: get the series with one difference
par(mfrow = c(1, 1))
plot(diff(gtemp), type = "o", main = "first difference")

# d) compare acfs of the original series, the detrended series 
# with the differences, and the detrended series with the residuals
par(mfrow = c(3, 1))
acf(gtemp, 50, main = "global temperatures")
acf(resid(fit), 50, main = "detrended")
acf(diff(gtemp), 50, main = "first difference")
