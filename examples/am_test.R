n <- 20
x <- sort(10 * runif(n))
y <- sort(15 * runif(n))
z <- 2 * exp(x) + 5 * y ^ 2 + 1 + rnorm(n, 0, 10)
data <- data.frame(x, y, z)

model <- lm(z ~ exp(x) + I(y ^ 2), data = data)
summary(model)

data_model <- partial(model, pred.var = "x")
plotPartial(data_model)

pdp_approx <- function(x, pdp) {
  normal_approx(x, pdp[[1]], pdp[[2]], d = 3)
}
plot(data_model[[1]], data_model[[2]], type = "l")
plot(function(x) pdp_approx(x, data_model), min(data_model[[1]]), max(data_model[[1]]), add = TRUE, col = "red")
lines(spline(data_model[[1]], data_model[[2]]), col = "red")
