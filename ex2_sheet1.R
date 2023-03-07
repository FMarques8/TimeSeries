# script for exercise 2 of sheet 1

w <- rnorm(500, 0, 1)
par(mfrow = c(2, 1))
plot.ts(w, main = "Gaussian white noise")

# samples measures
sample_mean <- mean(w)
sample_acf <- acf(w, lag = 20, type = "correlation"); sample_acf[20]

# for 50 samples
w <- rnorm(50, 0, 1)
par(mfrow = c(2, 1))
plot.ts(w, main = "Gaussian white noise")

# samples measures
sample_mean <- mean(w)
sample_acf <- acf(w, lag = 20, type = "correlation"); sample_acf[20]
