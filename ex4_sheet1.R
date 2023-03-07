# ex4 of sheet 1
# autoregressive model

n <- 500
x <- numeric(n)
w <- rnorm(n + 2)

x[0:1] <- w[0:1]

for (t in 3: n) {
    x[t] <- w[t] + x[t - 1] - 0.9 * x[t - 2]
}

par(mfrow = c(3, 1))
plot.ts(w, main = "gaussian white noise")
plot.ts(x, main = "autoregressive model")
acf(x, type = "correlation", main = "acf for the autoregressive model")

y <- numeric(n)
y[0:1] <- w[0:1]

for (t in 3: n) {
    y[t] <- w[t] + 0.2 * y[t - 1] - 0.4 * y[t - 2]
}

par(mfrow = c(3, 1))
plot.ts(w, main = "gaussian white noise")
plot.ts(y, main = "autoregressive model")
acf(y, type = "correlation", main = "acf for the autoregressive model")
