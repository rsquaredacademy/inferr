context('runs test')

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    resid <- tibble(residual = residuals(reg))
    k <- infer_runs_test(resid, residual)
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

test_that('output from runs test matches the expected result', {

  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- tibble(residual = residuals(reg))
  k <- infer_runs_test(resid, residual, drop = TRUE)
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

test_that('output from runs test matches the expected result', {

  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- tibble(residual = residuals(reg))
  k <- infer_runs_test(resid, residual, split = TRUE)
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

test_that('output from runs test matches the expected result', {

  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- tibble(residual = residuals(reg))
  k <- infer_runs_test(resid, residual, mean = TRUE)
    expect_equal(k$n, 32)
    expect_equal(k$threshold, -1.127570e-16)
    expect_equal(k$n_above, 13)
    expect_equal(k$n_below, 19)
    expect_equal(k$mean, 1.643750e+01)
    expect_equal(round(k$var, 2), 7.19)
    expect_equal(k$n_runs, 11)
    expect_equal(round(k$z, 2), -2.03)
    expect_equal(k$p, 4.257089e-02 )

})

test_that('output from runs test matches the expected result', {

  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- tibble(residual = residuals(reg))
  k <- infer_runs_test(resid, residual, threshold = 0)
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

test_that('output from runs test is as expected', {

  x <- cat("Runs Test
 Total Cases:  32
 Test Value :  -0.9630856
 Cases < Test Value:  16
 Cases > Test Value:  16
 Number of Runs:  11
 Expected Runs:  17
 Variance (Runs):  7.741935
 z Statistic:  -2.156386
 p-value:  0.03105355")

  reg <- lm(mpg ~ disp, data = mtcars)
  resid <- tibble(residual = residuals(reg))
  expect_equivalent(print(infer_runs_test(resid, residual)), x)

})
