# exercise
knots <- c(1, 2)
x <- 2 * runif(10)
x
b0_spline(x, knots)
plot(function(x) b0_spline(x, knots = 1:2), 0, 3)
plot(function(x) b0_spline(x, knots = 1:2, TRUE), 0, 3)


spline_plot(1:4)
spline_plot(c(0, 0, 1, 1))
spline_plot(c(0, 1, 1, 1))
spline_plot(c(0, 0, 0, 1))
spline_plot(c(0, 0, 1, 2))
spline_plot(c(0, 1, 1, 2))
spline_plot(c(0, 1, 2, 2))
spline_plot(c(0, 0, 0, 0))

# testing
spline_plot(c(0, 0, 0, 1))
plot(function(x) (1 - x) ^ 2, add = TRUE)

spline_plot(c(0, 0, 1, 1))
plot(function(x) 2* x *(1 - x), add = TRUE)


#quadratic bernstein
spline_plot(c(0,0,0,1))
spline_plot(c(0,0,1,1))
spline_plot(c(0,1,1,1))


# cubic bernstein
spline_plot(c(0,0,0,0,1))
spline_plot(c(0,0,0,1,1))
spline_plot(c(0,0,1,1,1))
spline_plot(c(0,1,1,1,1))

spline_j_plot(c(1:5), j = 1, d = 1)
spline_j_plot(c(1:5), j = 2, d = 1)
spline_j_plot(c(1:5), j = 3, d = 1)

spline_j_plot(c(1, 1:5, 5), j = 1, d = 1)
spline_j_plot(c(1, 1:5, 5), j = 2, d = 1)
spline_j_plot(c(1, 1:5, 5), j = 3, d = 1)
spline_j_plot(c(1, 1:5, 5), j = 4, d = 1)
spline_j_plot(c(1, 1:5, 5), j = 5, d = 1)

spline_j_plot(c(1, 1:5, 5), j = 1, d = 2)
spline_j_plot(c(1, 1:5, 5), j = 2, d = 2)
spline_j_plot(c(1, 1:5, 5), j = 3, d = 2)
spline_j_plot(c(1, 1:5, 5), j = 4, d = 2)
spline_j_plot(c(1, 1:5, 5), j = 5, d = 2)

# wniosek:
# Gdy mamy n knotów i d wymiar to baza ma (n - d - 1) splinów
# Gdy mamy spline d potrzebuje on d + 1 knotow


# more knots:
knots <- sort(rnorm(10, 0, 5))
knots <- 1:10
for (i in 1:7) {
  plot(function(x) b_spline(x, j = i, d = 2, knots = knots), min(knots) - 1, max(knots) + 1)
}

### interpolation
n <- 5
x <- sort(rnorm(n, 5, 10))
y <- rnorm(n, 5, 3)
knots <- c(x[1], x, x[n])
b1 <- function(x) {
  M <- (purrr::map_dfr(1:(length(knots) - 2), ~ as.data.frame(t(b_spline(x, ., 1, knots)))))
  t(y) %*% as.matrix(M)
}

plot(x, y)
plot(b1, min(x), max(x), add = TRUE)

x <- c(2, 4, 5)
n <- length(x)
d <- 2
knots <- c(rep(x[1], d), x, rep(x[n], d))
b_spline_matrix(c(2, 4, 5), 2, knots)

x <- rnorm(10, 5, 4)
y <- rnorm(10, 2, 1)
plot(x, y)
plot(function(val) b1_spline_itp(val, x, y), min(x), max(x), add = TRUE)
plot(splinefun(x, y, method = "periodic"), min(x), max(x), add = TRUE, col = "blue")

x <- 0:3
f <- function(x) {
  sqrt(2) * sin(pi * x / 2)
}

g <- function(x) {
  sin(pi * x / 2)
}

f <- function(x) {
 sqrt(2) * sin(pi * x / 2)
}
h <- function(x) {
  2 * sin(pi * x / 2)
}


d <- 2

#testing wtf
x <- 0:3
n <- length(x)
knots <- c(rep(x[1], d), x, rep(x[n], d))
d <- 2
v <- seq(min(knots), max(knots), by = 0.01)
v_y <-
  0 * b_spline(v, j = 1, d, knots) +
  1 * b_spline(v, j = 2, d, knots) +
  1 * b_spline(v, j = 3, d, knots) -
  1 * b_spline(v, j = 4, d, knots) -
  sqrt(2) * b_spline(v, j = 5, d, knots)

spline_j_plot(knots, j = 1, d)
lines(v, b_spline(v, j = 1, d, knots), col = "red")
lines(v, (1 - v) ^ 2, col = "blue")

spline_j_plot(knots, j = 2, d)
lines(v, b_spline(v, j = 2, d, knots), col = "red")
lines(v, v * (2 - 1.5 * v) * between(v, 0, 1) + 0.5 * (2 - v) ^ 2 * between(v, 1, 2), col = "blue")

spline_j_plot(knots, j = 3, d)
lines(v, b_spline(v, j = 3, d, knots), col = "red")
lines(v, v ^ 2 / 2 * between(v, 0, 1) + (0.75 - (v - 1.5) ^ 2) * between(v, 1, 2) + (3 - v) ^ 2 / 2 * between(v, 2, 3), col = "blue")

spline_j_plot(knots, j = 4, d)
lines(v, b_spline(v, j = 4, d, knots), col = "red")

spline_j_plot(knots, j = 5, d)
lines(v, b_spline(v, j = 5, d, knots), col = "red")

g <- function(x) sin(pi * x / 3)
plot(g, 0, 3)
plot(function(val) vdsa_2_approx(val, x, f, d), min(x), max(x), add = TRUE, col = "blue")
lines(v, v_y, col = "red")
lines(vdsa_approx_polygon(x, f, d)[[1]], vdsa_approx_polygon(x, f, d)[[2]], col = "green")

x <- 0:4
m <- length(x)
y <- rnorm(5, 2, 2)
#d <- 2
#knots <- c(rep(x[1], d), x, x[m])
#spline_j_plot(knots, m, 2)
plot(x, y)
plot(function(v) b_spline_interpolation(v, x, y), min(x), max(x), add = TRUE)

### approx
x <- rnorm(100, 5, 10)
y <- rnorm(100, 2, 2)
d <- 3
plot(x, y)
plot(function(val) normal_approx(val, x, y, d), min(x), max(x), add = TRUE)

f <- function(x) sin(x)
a <- -4
b <- 4
n <- 10
d <- 2
plot(f, a, b)
plot(function(val) vdsa_d_approx(val, a, b, f, 50, d), min(x), max(x), add = TRUE)

