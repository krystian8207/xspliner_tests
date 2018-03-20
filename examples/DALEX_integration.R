library(breakDown)
library(pdp)
library(randomForest)
library(magrittr)
library(xspliner)
library(DALEX)

HR_rf_model <- randomForest(factor(left)~., data = breakDown::HR_data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = HR_data,
                         predict_function = function(model, x) predict(model, x, type = "prob")[,2])
expl_rf  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "pdp", which.class = 2, prob = TRUE)
expl_rf_ale  <- single_variable(explainer_rf, variable = "satisfaction_level", type = "ale", which.class = 2, prob = TRUE)
expl_rf

single_variable_explainer <- expl_rf
model_spline_on_explainer <- function(single_variable_explainer, method) {
  x <- single_variable_explainer$x
  y <- single_variable_explainer$y
  mgcv::gam(y ~ s(x, bs = method))
}

make_explainer_spline_function <- function(explainer_spline_model) {
  model <- explainer_spline_model
  function(val) {
    data <- data.frame(x = val)
    predict.gam(model, data)
  }
}

spline_model <- model_spline_on_explainer(single_variable_explainer, "tp")

plot_explainer_spline_function <- function(spline_model) {
  spline <- make_explainer_spline_function(spline_model)
  data <- single_variable_explainer %>%
    mutate(spline_val = spline(x))
  plot(single_variable_explainer) + geom_line(data = data, aes(x = x, y = spline_val))
}

plot_explainer_spline_function(spline_model)
