#' @export
b0_spline <- function(x, knots, include_right = FALSE) {
  if (include_right) (x >= knots[1] & x <= knots[2]) * 1 else (x >= knots[1] & x < knots[2]) * 1
}

#' @export
b_spline <- function(x, j = 1, d, knots) {x
  if (missing(d)) {
    d = length(knots) - 2
  }
  ifelse(x < knots[j] | x > knots[j + d + 1], 0,
    if (d == 0) {
      b0_spline(x, knots[c(j, j + 1)], knots[j + 1] == knots[length(knots)])
    } else {
      if (knots[j] == knots[j + d + 1]) {
        return(x * 0)
      }
      if (knots[j] < knots[j + d] && knots[j + 1] == knots[j + d + 1]) {
        return((x - knots[j]) / (knots[j + d] - knots[j]) * b_spline(x, j, d - 1, knots))
      }
      if (knots[j] == knots[j + d] && knots[j + 1] < knots[j + d + 1]) {
        return((knots[j + d + 1] - x) / (knots[j + d + 1] - knots[j + 1]) * b_spline(x, j + 1, d - 1, knots))
      }
      return(
        (x - knots[j]) / (knots[j + d] - knots[j]) * b_spline(x, j, d - 1, knots) +
          (knots[j + d + 1] - x) / (knots[j + d + 1] - knots[j + 1]) * b_spline(x, j + 1, d - 1, knots)
      )
    })
}

#' @export
spline_plot <- function(knots) {
  plot(function(x) b_spline(x, knots = knots), min(knots) - 1, max(knots) + 1)
}

#' @export
spline_j_plot <- function(knots, j = 1, d = 1) {
  plot(function(x) b_spline(x, knots = knots, j = j, d = d), min(knots) - 1, max(knots) + 1)
}

#' @export
b_spline_matrix <- function(x, d, knots) {
  spline_base_idx <- 1:(length(knots) - d - 1)
  M <- as.matrix(
    purrr::map_dfr(
      spline_base_idx,
      ~ as.data.frame(t(b_spline(x, ., d, knots)))
    )
  )
  colnames(M) <- paste("x =", x)
  rownames(M) <- paste0("B", spline_base_idx)
  return(M)
}

#' @export
b1_spline_itp <- function(val, x, y) {
  y <- y[order(x)]
  x <- sort(x)
  n <- length(x)
  d <- 1
  knots <- c(rep(x[1], d), x, rep(x[n], d))
  t(y) %*% b_spline_matrix(val, d, knots)
}

#' @export
b_spline_interpolation <- function(val, x, y, knots = NULL, condition = "free") {
  y <- y[order(x)]
  x <- sort(x)
  m <- length(x)
  d <- 3
  if (is.null(knots) && condition == "free") {
    knots <- c(rep(x[1], d + 1), x[3:(m - 2)], rep(x[m], d + 1))
  }
  if (!all(knots[1:m] < knots[(d + 2):(m + d + 1)])) stop("knots are wrong xd")
  M <- t(b_spline_matrix(x, d, knots))
  if (!all(diag(M) > 0)) stop("spline matrix is not singular")
  c <- solve(M, y)
  t(c) %*% b_spline_matrix(val, d, knots)
}




