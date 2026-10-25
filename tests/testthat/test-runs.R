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
