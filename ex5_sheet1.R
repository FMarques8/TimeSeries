# ex5 of sheet 1
set.seed(5)

n <- 10

x_1 <- 2 * rbinom(n + 1, 1, 0.5) - 1
# To obtain 5 + x_t - 0.7 x_{t-1}, where x_t is 1 or -1 depending on a coin toss
y_1 <- 5 + filter(x_1, sides = 1, filter = c(1, -0.7))[-1]

par(mfrow = c(2, 1))
plot.ts(y_1, main = "Exercise 5, regular distance")
plot.ts(y_1, type = "s", main = "Trajectory in squares")

sd(y_1)

acf_for_n <- function(n) {
    x_1 <- 2 * rbinom(n + 1, 1, 0.5) - 1
    y_1 <- 5 + filter(x_1, sides = 1, filter = c(1, -0.7))[-1]
    acf(y_1, lag.max = 6, plot = FALSE)
}

acf_for_n(100)
acf_for_n(1000)
acf_for_n(10000)
acf_for_n(100000)
