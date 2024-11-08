test_that("output from ifr_os_var_test matches the expected result", {
  k <- ifr_os_var_test(mtcars, mpg, 0.3)
  expect_equal(k$n, 32)
  expect_equal(k$sd, 0.3)
  expect_equal(k$sigma, 6.0269)
  expect_equal(k$se, 1.0654)
  expect_equal(round(k$chi, 2), 12511.64)
  expect_equal(k$df, 31)
  expect_equal(k$p_lower, 1)
  expect_equal(k$p_upper, 0)
  expect_equal(k$p_two, 0)
  expect_equal(k$xbar, 20.0906)
  expect_equal(k$c_lwr, 3.8737)
  expect_equal(k$c_upr, 10.6527)
  expect_equal(k$conf, 0.95)
  expect_equal(k$var_name, "mpg")
  expect_equal(k$type, "both")
})


test_that("ifr_os_var_test returns appropriate errors", {
  expect_error(
    ifr_os_var_test(hsb, race, 0.3),
    "x must be numeric"
  )
  expect_error(
    ifr_os_var_test(mtcars, mpg, "0.3"),
    "sd must be numeric"
  )
  expect_error(
    ifr_os_var_test(mtcars, mpg, 0.3, "0.95"),
    "confint must be numeric"
  )
})


test_that("output from one sample variance test is as expected when alternative is less", {
  expect_snapshot(ifr_os_var_test(mtcars, mpg, 5, alternative = "less"))
})

test_that("output from one sample variance test is as expected when alternative is greater", {
  expect_snapshot(ifr_os_var_test(mtcars, mpg, 5, alternative = "greater"))
})

test_that("output from one sample variance test is as expected when alternative is both", {
  expect_snapshot(ifr_os_var_test(mtcars, mpg, 5, alternative = "both"))
})

test_that("output from one sample variance test is as expected when alternative is all", {
  expect_snapshot(ifr_os_var_test(mtcars, mpg, 5, alternative = "all"))
})
