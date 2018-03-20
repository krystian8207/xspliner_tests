#' @export
between <- function(x, down, up, right = FALSE) {
  if (right) 1 * (x > down & x < up) else 1 * (x > down & x <= up)
}
