test_that("output from ifr_ts_paired_ttest matches expected output", {

  k <- ifr_ts_paired_ttest(mtcars, mpg, qsec)
  expect_equal(k$Obs, 32)
  expect_equal(round(unname(k$b[[1]]), 2), c(20.09, 17.85, 2.24))
  expect_equal(round(unname(k$b[[2]]), 2), c(6.03, 1.79, 5.52))
  expect_equal(round(unname(k$b[[3]]), 2), c(1.07, 0.32, 0.98))
  expect_equal(unlist(k$conf_int1), c(17.92, 22.26))
  expect_equal(unlist(k$conf_int2), c(17.2, 18.49))
  expect_equal(unlist(k$conf_int_diff), c(0.25, 4.23))
  expect_equal(k$corr, 0.42)
  expect_equal(k$corsig, 0.02)
  expect_equal(unname(k$tstat), 2.2964)
  expect_equal(round(unname(k$p_lower), 3), 0.986)
  expect_equal(round(unname(k$p_upper), 3), 0.014)
  expect_equal(round(unname(k$p_two_tail), 3), 0.029)
  expect_equal(k$var_names, c("mpg", "qsec"))
  expect_equal(k$xy, c("mpg - qsec"))
  expect_equal(k$alternative, c("both"))
  expect_equal(k$df, 31)
  expect_equal(k$confint, 0.95)
})


test_that("output from paired sample t test is as expected when alternative is less", {
  expect_snapshot(ifr_ts_paired_ttest(
    hsb, read, write,
    alternative = "less"
  ))
})

test_that("output from paired sample t test is as expected when alternative is greater", {
  expect_snapshot(ifr_ts_paired_ttest(
    hsb, read, write,
    alternative = "greater"
  ))
})

test_that("output from paired sample t test is as expected when alternative is both", {
  expect_snapshot(ifr_ts_paired_ttest(
    hsb, read, write,
    alternative = "both"
  ))
})

test_that("output from paired sample t test is as expected when alternative is all", {
  expect_snapshot(ifr_ts_paired_ttest(
    hsb, read, write,
    alternative = "all"
  ))
})
