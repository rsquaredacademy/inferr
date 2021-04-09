context("two sample variance test")

mtcarz <- mtcars
mtcarz$vs <- as.factor(mtcarz$vs)

test_that("output from infer_ts_var_test matches expected result", {
  k <- infer_ts_var_test(mtcarz, mpg, group_var = vs)
  expect_equal(k$f, 0.5151)
  expect_equal(k$lower, 0.0999)
  expect_equal(k$upper, 0.9001)
  expect_equivalent(as.vector(k$vars), c(14.90, 28.93))
  expect_equivalent(as.vector(k$avgs), c(16.62, 24.56))
  expect_equivalent(as.vector(k$sds), c(3.86, 5.38))
  expect_equivalent(as.vector(k$ses), c(0.91, 1.44))
  expect_equal(k$avg, 20.09)
  expect_equal(k$sd, 6.03)
  expect_equal(k$se, 1.07)
  expect_equal(k$len, 32)
  expect_equivalent(as.vector(k$lens), c(18, 14))
  expect_equal(as.vector(k$n1), 17)
  expect_equal(as.vector(k$n2), 13)
  expect_equivalent(k$type, "less")
  expect_equivalent(k$lev, c("0", "1"))
})

test_that("output from infer_ts_var_test matches expected result", {
  k <- infer_ts_var_test(mtcarz, mpg, qsec)
  expect_equal(k$f, 11.3756)
  expect_equal(k$lower, 1)
  expect_equal(k$upper, 0)
  expect_equivalent(as.vector(k$vars), c(36.32, 3.19))
  expect_equivalent(as.vector(k$avgs), c(20.09, 17.85))
  expect_equivalent(as.vector(k$sds), c(6.03, 1.79))
  expect_equivalent(as.vector(k$ses), c(1.07, 0.32))
  expect_equal(k$avg, 18.97)
  expect_equal(k$sd, 4.55)
  expect_equal(k$se, 0.57)
  expect_equal(k$len, 64)
  expect_equivalent(as.vector(k$lens), c(32, 32))
  expect_equal(unname(k$n1), 31)
  expect_equal(unname(k$n2), 31)
  expect_equivalent(k$type, "less")
  expect_equivalent(k$lev, c("mpg", "qsec"))
})


test_that("output from 2 sample variance test is as expected when alternative is less", {
  x <- cat("               Variance Ratio Test
--------------------------------------------------
  Group      Obs    Mean     Std. Err.    Std. Dev.
--------------------------------------------------
  read      200    52.23      0.72         10.25
 write      200    52.77      0.67         9.48
--------------------------------------------------
 combined    400    52.5       0.49         9.86
--------------------------------------------------
           Lower Tail Test
           ---------------
      ratio = sd(read) / (write)
            Ho: ratio = 1
            Ha: ratio < 1
        Variance Ratio Test
------------------------------------
   F       Num DF    Den DF      p
------------------------------------
 1.1701     199       199      0.8656
------------------------------------")

  expect_equivalent(print(infer_ts_var_test(hsb, read, write, alternative = "less")), x)
})

test_that("output from 2 sample variance test is as expected when alternative is greater", {
  x <- cat("               Variance Ratio Test
--------------------------------------------------
  Group      Obs    Mean     Std. Err.    Std. Dev.
--------------------------------------------------
  read      200    52.23      0.72         10.25
 write      200    52.77      0.67         9.48
--------------------------------------------------
 combined    400    52.5       0.49         9.86
--------------------------------------------------
           Upper Tail Test
           ---------------
            Ho: ratio = 1
            Ha: ratio > 1
        Variance Ratio Test
------------------------------------
   F       Num DF    Den DF      p
------------------------------------
 1.1701     199       199      0.1344
------------------------------------")

  expect_equivalent(print(infer_ts_var_test(hsb, read, write, alternative = "greater")), x)
})

test_that("output from 2 sample variance test is as expected when alternative is all", {
  x <- cat("               Variance Ratio Test
--------------------------------------------------
  Group      Obs    Mean     Std. Err.    Std. Dev.
--------------------------------------------------
  read      200    52.23      0.72         10.25
 write      200    52.77      0.67         9.48
--------------------------------------------------
 combined    400    52.5       0.49         9.86
--------------------------------------------------
                Variance Ratio Test
--------------------------------------------------
        F              Num DF           Den DF
--------------------------------------------------
      1.1701            199              199
--------------------------------------------------
       Null & Alternate Hypothesis
----------------------------------------
       ratio = sd(read) / (write)
              Ho: ratio = 1
    Ha: ratio < 1        Ha: ratio > 1
  Pr(F < f) = 0.8656   Pr(F > f) = 0.1344
----------------------------------------")

  expect_equivalent(print(infer_ts_var_test(hsb, read, write, alternative = "all")), x)
})
