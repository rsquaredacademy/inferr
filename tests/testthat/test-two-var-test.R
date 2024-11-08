mtcarz <- mtcars
mtcarz$vs <- as.factor(mtcarz$vs)

test_that("output from ifr_ts_var_test matches expected result", {
  k <- ifr_ts_var_test(mtcarz, mpg, group_var = vs)
  expect_equal(k$f, 0.5151)
  expect_equal(k$lower, 0.0999)
  expect_equal(k$upper, 0.9001)
  expect_equal(as.vector(k$vars), c(14.90, 28.93))
  expect_equal(as.vector(k$avgs), c(16.62, 24.56))
  expect_equal(as.vector(k$sds), c(3.86, 5.38))
  expect_equal(as.vector(k$ses), c(0.91, 1.44))
  expect_equal(k$avg, 20.09)
  expect_equal(k$sd, 6.03)
  expect_equal(k$se, 1.07)
  expect_equal(k$len, 32)
  expect_equal(as.vector(k$lens), c(18, 14))
  expect_equal(as.vector(k$n1), 17)
  expect_equal(as.vector(k$n2), 13)
  expect_equal(k$type, "less")
  expect_equal(k$lev, c("0", "1"))
})

test_that("output from ifr_ts_var_test matches expected result", {
  k <- ifr_ts_var_test(mtcarz, mpg, qsec)
  expect_equal(k$f, 11.3756)
  expect_equal(k$lower, 1)
  expect_equal(k$upper, 0)
  expect_equal(as.vector(k$vars), c(36.32, 3.19))
  expect_equal(as.vector(k$avgs), c(20.09, 17.85))
  expect_equal(as.vector(k$sds), c(6.03, 1.79))
  expect_equal(as.vector(k$ses), c(1.07, 0.32))
  expect_equal(k$avg, 18.97)
  expect_equal(k$sd, 4.55)
  expect_equal(k$se, 0.57)
  expect_equal(k$len, 64)
  expect_equal(as.vector(k$lens), c(32, 32))
  expect_equal(unname(k$n1), 31)
  expect_equal(unname(k$n2), 31)
  expect_equal(k$type, "less")
  expect_equal(k$lev, c("mpg", "qsec"))
})


test_that("output from 2 sample variance test is as expected when alternative is less", {
  expect_snapshot(ifr_ts_var_test(hsb, read, write, alternative = "less"))
})

test_that("output from 2 sample variance test is as expected when alternative is greater", {
  expect_snapshot(ifr_ts_var_test(hsb, read, write, alternative = "greater"))
})

test_that("output from 2 sample variance test is as expected when alternative is all", {
  expect_snapshot(ifr_ts_var_test(hsb, read, write, alternative = "all"))
})
