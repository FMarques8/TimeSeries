#data
par(mfrow = c(1, 1))
jj

# Analyze the data - do whats below!!!!!!!!
ts.plot(jj, type = "o", main =" Quarter earnings per action")
trend <- time(jj) - 1970 # to be centered in time

??stl #seasonal decomposition of time series
results <- stl(jj, s.window = "per") # using STL decomposition

results # seasonal, trend and remainder decomposition
plot(results)

# b)model: beta*t+alpha_1*q_1(t)++alpha_2*q_2(t)+alpha_3*q_3(t)+alpha_4*q_4(t)+w_t
time(jj)
ts.plot(trend)
Q <- factor(cycle(jj)) # in order to use quarters as factors

# to apply regression model with intercept
reg <- lm(jj ~ 0 + trend + Q, na.action = NULL)

model.matrix(reg)
summary(reg)

# c) without intercept
reg <- lm(log(jj) ~ 0 + trend + Q, na.action = NULL)

par(mfrow = c(3, 1))
ts.plot(jj, lty = "dashed"); lines(fitted(reg), lwd = 2)

# residuals- is white noise?
ts.plot(resid(reg), type = "o", main = "residuals") #w_t_est
acf(resid(reg))

# with intercept
reg <- lm(jj ~ 0 + trend + Q, na.action = NULL)

par(mfrow = c(3, 1))
ts.plot(jj, lty = "dashed"); lines(fitted(reg), lwd = 2)

# residuals- is white noise?
ts.plot(resid(reg), type = "o", main = "residuals") #w_t_est
acf(resid(reg))

# extra exercise
## generate 4 random walks with
# drift 1.4, 100 iterations, delta=0.01, sigma_w=1
# model x_t=delta+x_{t-1}+w_t

w <- rnorm(100, 0, 1)
wd <- w + 1.4
xd <- cumsum(wd)
plot.ts(xd, main = "random walk")
lines(xd)
lines(1.4 * (1:100), lty = "dashed")

par(new = TRUE)
w <- rnorm(100, 0, 1)
wd <- w + 0.01
xd <- cumsum(wd)
plot.ts(xd, main = "random walk", labels = FALSE)
lines(xd)
lines(0.01 * (1:100), lty = "dashed")

par(new = TRUE)
w <- rnorm(100, 0, 1)
wd <- w + 0.01
xd <- cumsum(wd)
plot.ts(xd, main = "random walk", labels = FALSE)
lines(xd)

par(new = TRUE)
w <- rnorm(100, 0, 1)
wd <- w + 0.01
xd <- cumsum(wd)
plot.ts(xd, main = "random walk", labels = FALSE)
lines(xd)

