context("paired t test")

test_that("output from infer_ts_paired_ttest matches expected output", {

  k <- infer_ts_paired_ttest(mtcars, mpg, qsec)
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
  expect_equivalent(k$var_names, c("mpg", "qsec"))
  expect_equivalent(k$xy, c("mpg - qsec"))
  expect_equivalent(k$alternative, c("both"))
  expect_equal(k$df, 31)
  expect_equal(k$confint, 0.95)
})


test_that("output from paired sample t test is as expected when alternative is less", {
  x <- cat("                         Paired Samples Statistics
---------------------------------------------------------------------------
Variables    Obs    Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------
   read       200    52.23      0.72         10.25         50.8      53.66
   write      200    52.77      0.67         9.48         51.45      54.09
---------------------------------------------------------------------------
   diff       200    -0.55      0.63         8.89         -1.79       0.69
---------------------------------------------------------------------------
         Paired Samples Correlations
-------------------------------------------
  Variables      Obs    Correlation    Sig.
 read & write    200       0.60        0
-------------------------------------------
          Paired Samples Test
          -------------------
      Ho: mean(read - write) = 0
      Ha: mean(read - write) < 0
---------------------------------------
  Variables        t       df     Sig.
---------------------------------------
 read - write    -0.873    199    0.192
---------------------------------------")

  expect_equivalent(print(infer_ts_paired_ttest(
    hsb, read, write,
    alternative = "less"
  )), x)
})

test_that("output from paired sample t test is as expected when alternative is greater", {
  x <- cat("                         Paired Samples Statistics
---------------------------------------------------------------------------
Variables    Obs    Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------
   read       200    52.23      0.72         10.25         50.8      53.66
   write      200    52.77      0.67         9.48         51.45      54.09
---------------------------------------------------------------------------
   diff       200    -0.55      0.63         8.89         -1.79       0.69
---------------------------------------------------------------------------
         Paired Samples Correlations
-------------------------------------------
  Variables      Obs    Correlation    Sig.
 read & write    200       0.60        0
-------------------------------------------
          Paired Samples Test
          -------------------
      Ho: mean(read - write) = 0
      Ha: mean(read - write) > 0
---------------------------------------
  Variables        t       df     Sig.
---------------------------------------
 read - write    -0.873    199    0.808
---------------------------------------")

  expect_equivalent(print(infer_ts_paired_ttest(
    hsb, read, write,
    alternative = "greater"
  )), x)
})

test_that("output from paired sample t test is as expected when alternative is both", {
  x <- cat("                         Paired Samples Statistics
---------------------------------------------------------------------------
Variables    Obs    Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------
   read       200    52.23      0.72         10.25         50.8      53.66
   write      200    52.77      0.67         9.48         51.45      54.09
---------------------------------------------------------------------------
   diff       200    -0.55      0.63         8.89         -1.79       0.69
---------------------------------------------------------------------------
         Paired Samples Correlations
-------------------------------------------
  Variables      Obs    Correlation    Sig.
 read & write    200       0.60        0
-------------------------------------------
          Paired Samples Test
          -------------------
      Ho: mean(read - write) = 0
      Ha: mean(read - write) ~= 0
---------------------------------------
  Variables        t       df     Sig.
---------------------------------------
 read - write    -0.873    199    0.384
---------------------------------------")

  expect_equivalent(print(infer_ts_paired_ttest(
    hsb, read, write,
    alternative = "both"
  )), x)
})

test_that("output from paired sample t test is as expected when alternative is all", {
  x <- cat("                         Paired Samples Statistics
---------------------------------------------------------------------------
Variables    Obs    Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------
   read       200    52.23      0.72         10.25         50.8      53.66
   write      200    52.77      0.67         9.48         51.45      54.09
---------------------------------------------------------------------------
   diff       200    -0.55      0.63         8.89         -1.79       0.69
---------------------------------------------------------------------------
         Paired Samples Correlations
-------------------------------------------
  Variables      Obs    Correlation    Sig.
 read & write    200       0.60        0
-------------------------------------------
                Ho: mean(read - write) = mean(diff) = 0
   Ha: mean(diff) < 0      Ha: mean(diff) ~= 0       Ha: mean(diff) > 0
       t = -0.873               t = -0.873               t = -0.873
     P < t = 0.192           P > |t| = 0.384           P > t = 0.808")

  expect_equivalent(print(infer_ts_paired_ttest(
    hsb, read, write,
    alternative = "all"
  )), x)
})

