library("latex2exp")
set.seed(2000)

#------------------------------------------------------------------------------
#REALIZACIONES

n <- 1000
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rbinom(n, size = 10, prob = 0.4)
x3 <- rchisq(n, df = 5, ncp = 0)
x4 <- rgamma(n, shape = 10, rate = 0.7)

#------------------------------------------------------------------------------
#EJERCICIO A: DENSIDAD

plot(density(x1, bw = "nrd0", kernel = "gaussian"), col = "red", lwd = 2, xlab = "x", ylab = "f(x)", main = TeX('$N(0, 1)$'), xlim = c(-5, 5), ylim = c(0, 0.4))

curve(add = TRUE, dnorm(x, mean = 0, sd = 1), col = "blue", lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO A: DISTRIBUCION

plot(ecdf(x1), col = "red", lwd = 2, xlab = "x", ylab = "F(x)", main = TeX('$N(0, 1)$'), xlim = c(-5, 5))

curve(add = TRUE, pnorm(x, mean = 0, sd = 1), col = "blue",lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO B: PROBABILIDAD

x <- 0:10

barplot(table(x2)/n, col = rgb(1, 0, 0, 0.5), lwd = 2, xlab = "x", ylab = "p(x)", main = TeX('$B(10, 0.4)$'), xlim = c(0, 11), ylim = c(0, 0.25))

barplot(add = TRUE, dbinom(x, size = 10, prob = 0.4), col = rgb(0, 0, 1, 0.5), lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO B: DISTRIBUCION

plot(ecdf(x2), col = "red", lwd = 2, xlab = "x", ylab = "F(x)", main = TeX('$B(10, 0.4)$'), xlim = c(0, 10))

points(add = TRUE, pbinom(x, size = 10, prob = 0.4), col = "blue",lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO C: DENSIDAD

plot(density(x3, bw = "ucv", kernel = "epanechnikov"), col = "red", lwd = 2, xlab = "x", ylab = "f(x)", main = TeX('$\\X^2_5$'), xlim = c(-2, 20), ylim = c(0, 0.15))

curve(add = TRUE, dchisq(x, df = 5, ncp = 0), col = "blue", lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO C: DISTRIBUCION

plot(ecdf(x3), col = "red", lwd = 2, xlab = "x", ylab = "F(x)", main = TeX('$\\X^2_5$'), xlim = c(-2, 20))

curve(add = TRUE, pchisq(x, df = 5, ncp = 0), col = "blue",lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO D: DENSIDAD

plot(density(x4, bw = "ucv", kernel = "epanechnikov"), col = "red", lwd = 2, xlab = "x", ylab = "f(x)", main = TeX('$Gamma (10, 0.7)$'), xlim = c(0, 35), ylim = c(0, 0.1))

curve(add = TRUE, dgamma(x, shape = 10, rate = 0.7), col = "blue", lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#EJERCICIO D: DISTRIBUCION

plot(ecdf(x4), col = "red", lwd = 2, xlab = "x", ylab = "F(x)", main = TeX('$Gamma (10, 0.7)$'), xlim = c(0, 35))

curve(add = TRUE, pgamma(x, shape = 10, rate = 0.7), col = "blue",lwd = 2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 2)
legend("topright", legend = c("Estimada", "Teórica"), col = c("red", "blue"), lwd = 2)

#------------------------------------------------------------------------------
#GRAFICOS Q-Q
#------------------------------------------------------------------------------
#EJERCICIO A

qqnorm(x1, col = "red", pch = 20, xlab = "Cuantiles teóricos", ylab = "Cuantiles estimados", main = "Gráfico Q-Q \n N(0, 1)")
qqline(x1)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

#------------------------------------------------------------------------------
#EJERCICIO B

qqnorm(x2, col = "red", pch = 20, xlab = "Cuantiles teóricos", ylab = "Cuantiles estimados", main = "Gráfico Q-Q \n B(10, 0.4)")
qqline(x2)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

#------------------------------------------------------------------------------
#EJERCICIO C

qqnorm(x3, col = "red", pch = 20, xlab = "Cuantiles teóricos", ylab = "Cuantiles estimados", main = TeX('Gráfico Q-Q $\\X^2_5$'), ylim = c(-5, 25))
qqline(x3)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

#------------------------------------------------------------------------------
#EJERCICIO D

qqnorm(x4, col = "red", pch = 20, xlab = "Cuantiles teóricos", ylab = "Cuantiles estimados", main = TeX('Gráfico Q-Q $Gamma (10, 0.7)$'), ylim = c(0, 35))
qqline(x4)

grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)

