#' @export
vdsa_2_approx <- function(val, x, f, method = "uniform") {
  if (d < 2) stop("d must be greater than 1")
  x <- sort(x)
  n <- length(x) + 1
  if (d == 2 && method == "points") {
    knots <- c(rep(x[1], d), x, rep(x[n - 1], d))
  }
  if (method == "uniform") {
    knots <- c(rep(x[1], d), x, rep(x[n - 1], d))
  }
  x_int <- (knots[2:(n + 1)] + knots[3:(n + 2)]) / 2
  y <- f(x_int)
  t(y) %*% b_spline_matrix(val, d, knots)
}

#' @export
vdsa_approx_polygon <- function(x, f, d) {
  if (d < 2) stop("d must be greater than 1")
  x <- sort(x)
  n <- length(x) + 1
  knots <- c(rep(x[1], d), x, rep(x[n - 1], d))
  x_int <- (knots[2:(n + 1)] + knots[3:(n + 2)]) / 2
  y <- f(x_int)
  return(list(x_int, y))
}

#' @export
normal_approx <- function(val, x, y, d, knots = NULL, base_dim = sqrt(length(x)), w = rep(1, length(x))) {
  y <- y[order(x)]
  x <- sort(x)
  if (is.null(knots)) {
    knots <- seq(min(x), max(x), length.out = base_dim + d + 1)
  }
  A <- t(b_spline_matrix(x, d, knots)) * sqrt(w)
  b <- y * sqrt(w)
  c <- solve(t(A) %*% A, t(A) %*% b)
  t(c) %*% b_spline_matrix(val, d, knots)
}

calc_avg_knots <- function(knots, d, n) {
  purrr::map_dbl(1:n, ~ mean(knots[(. + 1):(. + d)]))
}

#' @export
vdsa_d_approx <- function(val, a, b, f, n, d, method = "uniform") {
  if (method == "uniform") {
    knots <- seq(a, b, length.out = n + d + 1)
  }
  t_avg <- calc_avg_knots(knots, d, n)
  y <- f(t_avg)
  t(y) %*% b_spline_matrix(val, d, knots)
}
