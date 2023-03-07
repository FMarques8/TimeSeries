# ex 3 of sheet 1
# b)
# time series xt = 1/3 *(wt-1 + wt + wt+1)

n <- 500
x <- numeric(n)
w <- rnorm(n + 2)

for (t in 2:n + 1) x[t] <- 1 / 3 * (w[t - 1] + w[t] + w[t + 1])

par(mfrow = c(3, 1))
# Plot in the time series context
plot.ts(w, main = "the gaussian white noise")
plot.ts(x, main = "moving average of the gaussian white noise")
acf(x, type = "correlation")

# c) & d)
par(mfrow = c(1, 1))
acf_for_n <- function(n) {
    n <- 500
    x <- numeric(n)
    w <- rnorm(n + 2)
    for (t in 2:n) x[t] <- 1 / 3 * (w[t - 1] + w[t] + w[t + 1])
    acf(x) # Sample ACF
    print(acf(x, lag = 20, plot = FALSE)[20])
}

acf_for_n(500)
acf_for_n(50)
