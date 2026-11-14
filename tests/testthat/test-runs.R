test_that("output from runs test matches the expected result", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  k <- ifr_runs_test(resid, residual)
  expect_equal(k$n, 32)
  expect_equal(round(k$threshold, 3), -0.963)
  expect_equal(k$n_above, 16)
  expect_equal(k$n_below, 16)
  expect_equal(k$mean, 17)
  expect_equal(round(k$var, 3), 7.742)
  expect_equal(k$n_runs, 11)
  expect_equal(round(k$z, 3), -2.156)
  expect_equal(round(k$p, 3), 0.031)
})

test_that("output from runs test matches the expected result", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  k <- ifr_runs_test(resid, residual, drop = TRUE)
  expect_equal(k$n, 32)
  expect_equal(round(k$threshold, 3), -0.963)
  expect_equal(k$n_above, 16)
  expect_equal(k$n_below, 16)
  expect_equal(k$mean, 17)
  expect_equal(round(k$var, 3), 7.742)
  expect_equal(k$n_runs, 11)
  expect_equal(round(k$z, 3), -2.156)
  expect_equal(round(k$p, 3), 0.031)
})

test_that("output from runs test matches the expected result", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  k <- ifr_runs_test(resid, residual, split = TRUE)
  expect_equal(k$n, 32)
  expect_equal(round(k$threshold, 3), -0.963)
  expect_equal(k$n_above, 16)
  expect_equal(k$n_below, 16)
  expect_equal(k$mean, 17)
  expect_equal(round(k$var, 3), 7.742)
  expect_equal(k$n_runs, 11)
  expect_equal(round(k$z, 3), -2.156)
  expect_equal(round(k$p, 3), 0.031)
})

test_that("output from runs test matches the expected result", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  k <- ifr_runs_test(resid, residual, mean = TRUE)
  expect_equal(k$n, 32)
  expect_equal(k$threshold, -1.127570e-16)
  expect_equal(k$n_above, 13)
  expect_equal(k$n_below, 19)
  expect_equal(k$mean, 1.643750e+01)
  expect_equal(round(k$var, 2), 7.19)
  expect_equal(k$n_runs, 11)
  expect_equal(round(k$z, 2), -2.03)
  expect_equal(round(k$p, 2), 0.04) 
})

test_that("output from runs test matches the expected result", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  k <- ifr_runs_test(resid, residual, threshold = 0)
  expect_equal(k$n, 32)
  expect_equal(round(k$threshold, 3), 0)
  expect_equal(k$n_above, 13)
  expect_equal(k$n_below, 19)
  expect_equal(k$mean, 16.4375)
  expect_equal(round(k$var, 3), 7.19)
  expect_equal(k$n_runs, 11)
  expect_equal(round(k$z, 3), -2.028)
  expect_equal(round(k$p, 3), 0.043)
})

test_that("output from runs test is as expected", {
  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- data.frame(residual = residuals(reg))
  expect_snapshot(ifr_runs_test(resid, residual))
})

test_that("ifr_runs_test handles ties, missing values and degenerate samples (M5)", {
  expect_error(
    ifr_runs_test(data.frame(v = rep(2, 4)), v, drop = TRUE),
    "fewer than two observations remain"
  )
  expect_error(
    ifr_runs_test(data.frame(v = rep(2, 4)), v),
    "both above and below the threshold"
  )
  expect_error(
    ifr_runs_test(data.frame(v = c(1, NA)), v),
    "at least two observations"
  )
  expect_error(
    ifr_runs_test(data.frame(v = c(1, 2)), v),
    "variance of the number of runs"
  )

  # missing values are removed before the threshold is computed
  d <- data.frame(v = c(1, 2, NA, 4))
  k <- ifr_runs_test(d, v)
  n0 <- 2
  n1 <- 1
  N  <- n0 + n1
  exp_runs <- (2 * n0 * n1 / N) + 1
  var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - N)) / (N ^ 2 * (N - 1))

  expect_equal(k$n, 3)
  expect_equal(k$threshold, 2)
  expect_equal(k$n_below, n0)
  expect_equal(k$n_above, n1)
  expect_equal(k$n_runs, 2)
  expect_equal(k$mean, exp_runs)
  expect_equal(k$var, var_runs)
  expect_equal(k$z, (2 - exp_runs) / sqrt(var_runs))
  expect_equal(k$p, 2 * pnorm(abs(k$z), lower.tail = FALSE))
})

test_that("runs test keeps a representable upper-tail p-value (H12)", {
  d <- data.frame(v = c(rep(-1, 100), rep(1, 100)))
  k <- ifr_runs_test(d, v, threshold = 0)

  n0 <- 100
  n1 <- 100
  N  <- n0 + n1
  exp_runs <- (2 * n0 * n1 / N) + 1
  var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - N)) / (N ^ 2 * (N - 1))
  z        <- (2 - exp_runs) / sqrt(var_runs)
  ref_p    <- 2 * pnorm(abs(z), lower.tail = FALSE)

  expect_equal(k$n, 200)
  expect_equal(k$n_runs, 2)
  expect_equal(k$mean, exp_runs)
  expect_equal(k$var, var_runs)
  expect_equal(k$z, z)
  expect_gt(k$p, 0)
  expect_equal(k$p, ref_p, tolerance = 1e-12)
  expect_equal(k$p / ref_p, 1, tolerance = 1e-12)

  # nearby ordinary case with the same two-sided normal reference
  d2 <- data.frame(v = c(rep(-1, 10), rep(1, 10)))
  k2 <- ifr_runs_test(d2, v, threshold = 0)
  n0 <- 10
  n1 <- 10
  N  <- n0 + n1
  exp_runs <- (2 * n0 * n1 / N) + 1
  var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - N)) / (N ^ 2 * (N - 1))
  z2       <- (2 - exp_runs) / sqrt(var_runs)
  ref_p2   <- 2 * pnorm(abs(z2), lower.tail = FALSE)

  expect_equal(k2$n_runs, 2)
  expect_equal(k2$z, z2)
  expect_equal(k2$p, ref_p2, tolerance = 1e-12)
  expect_equal(k2$p / ref_p2, 1, tolerance = 1e-12)
})

test_that("ifr_runs_test reports n after dropping values equal to the threshold", {
  d <- data.frame(v = c(1, 2, 2, 3, 4, 4, 5, 6, 7, 8))

  k <- ifr_runs_test(d, v, drop = TRUE)

  expect_equal(k$n, 8)
  expect_equal(k$n, k$n_above + k$n_below)

  # independent computation of the z statistic on the retained values
  x  <- d$v[d$v != median(d$v)]
  n1 <- sum(x > 4)
  n0 <- sum(x < 4)
  N  <- n0 + n1
  runs <- 1 + sum(diff(x > 4) != 0)
  exp_runs <- (2 * n0 * n1 / N) + 1
  var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - N)) / (N ^ 2 * (N - 1))
  z        <- (runs - exp_runs) / sqrt(var_runs)

  expect_equal(k$n_runs, runs)
  expect_equal(k$z, z)
})
