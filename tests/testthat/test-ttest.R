test_that("output from ifr_os_t_test matches the expected output", {
  k <- ifr_os_t_test(mtcars, mpg, mu = 50, alternative = "less")
  expect_equal(k$mu, 50)
  expect_equal(k$n, 32)
  expect_equal(k$df, 31)
  expect_equal(k$Mean, 20.0906)
  expect_equal(k$stddev, 6.0269)
  expect_equal(k$std_err, 1.0654)
  expect_equal(k$test_stat, -28.073)
  expect_equal(k$confint, c(-Inf, 21.8974))
  expect_equal(k$mean_diff_l, -Inf)
  expect_equal(k$mean_diff_u, -28.1026)
  expect_equal(k$mean_diff, -29.9094)
  expect_equal(k$p_l, 6.592161e-24)
  expect_equal(k$p_u, 1)
  expect_equal(k$p, 1.31843219487798e-23)
  expect_equal(k$conf, 0.95)
  expect_equal(k$type, "less")
  expect_equal(k$var_name, "mpg")

  k <- ifr_os_t_test(mtcars, mpg, mu = 50, alternative = "greater")
  expect_equal(k$confint, c(18.2846, Inf))

  k <- ifr_os_t_test(mtcars, mpg, mu = 50, alternative = "both")
  expect_equal(k$confint, c(17.9181, 22.2639))
})

test_that("ifr_os_t_test throws the appropriate error", {
  expect_error(ifr_os_t_test(hsb, race, mu = 50), "x must be numeric")
  expect_error(ifr_os_t_test(mtcars, mpg, mu = "50"), "mu must be numeric")
  expect_error(ifr_os_t_test(mtcars, mpg, mu = 50, alpha = "0.05"), "alpha must be numeric")
})

test_that("output from one sample t test is as expected when alternative is less", {
  expect_snapshot(ifr_os_t_test(hsb, write, mu = 50, alternative = "less"))
})

test_that("output from one sample t test is as expected when alternative is greater", {
  expect_snapshot(ifr_os_t_test(hsb, write, mu = 50, alternative = "greater"))
})

test_that("output from one sample t test is as expected when alternative is both", {
  expect_snapshot(ifr_os_t_test(hsb, write, mu = 50, alternative = "both"))
})

test_that("output from one sample t test is as expected when alternative is all", {
  expect_snapshot(ifr_os_t_test(hsb, write, mu = 50, alternative = "all"))
})
