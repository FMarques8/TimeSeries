# ex3 of sheet 2
require(astsa)

par(mfrow = c(2, 1))

varve
ts.plot(varve, main = "sediments")
mean(varve)
var(varve)
length(varve)

# 2a)
x1varve <- window(varve, start = 1, end = 317) # window of the first half
mean(x1varve)
var(x1varve)

x2varve <- window(varve, start = 318) # second half
mean(x2varve)
var(x2varve)

# stabilize the variance
mean(log(varve))
var(log(varve)) # much lower → stabilized

plot(log(varve))

par(mfrow = c(2, 1))
plot(log(varve))
plot(varve)

hist(log(varve))
hist(varve)

# 2b)
plot(log(varve), main = "log sediment deposits")

# 2c)
par(mfrow = c(2, 1))
acf(varve, main = "sediment deposits")
acf(log(varve), main = "log sediment deposits")

# 2d) 
diff(log(varve))
plot(diff(log(varve)), main = "1st difference of log sediment deposits")
mean(diff(log(varve)))
var(diff(log(varve)))
acf(diff(log(varve)), main = "1st difference of log sediment deposits")
