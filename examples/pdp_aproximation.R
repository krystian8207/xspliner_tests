library(breakDown)
library(pdp)
library(randomForest)
library(magrittr)
library(xspliner)
HR_data %>% str
HR_rf_model <- randomForest(left ~ ., data = HR_data, ntree = 500)
pdp <- partial(HR_rf_model, pred.var = "satisfaction_level")

pdp_approx <- function(x, pdp) {
  normal_approx(x, pdp[[1]], pdp[[2]], d = 3)
}
plot(pdp[[1]], pdp[[2]], type = "l", ylim = c(-1, 1))
plot(function(x) pdp_approx(x, pdp), min(pdp[[1]]), max(pdp[[1]]), add = TRUE, col = "red")
lines(spline(pdp[[1]], pdp[[2]]), col = "green")

x <- pdp[[1]]
y <- pdp[[2]]

model <- lm(y ~ x)
plot(x, model$fitted.values, type = "l", col = "red")
plot(HR_data$satisfaction_level, HR_data$left)
lines(x, y, ylim = c(-1, 1), col = "green")

gam_model <- gam(y ~ s(x, bs = "ds"))
summary(gam_model)
# plot(gam_model, ylim = c(-1, 1))
plot(x, gam_model$fitted.values, type = "l", ylim = c(-1, 1))
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "cr"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "cs"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "cc"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "ps"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "cp"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "re"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "mfr"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

gam_model <- gam(y ~ s(x, bs = "gp"))
plot(gam_model)
lines(x, y, ylim = c(-1, 1))

