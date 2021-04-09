context("anova")

test_that("output from infer_oneway_anova matches the expected output", {
  k <- infer_oneway_anova(mtcars, mpg, cyl)
  expect_equal(k$between, 824.785)
  expect_equal(k$within, 301.263)
  expect_equal(k$total, 1126.048)
  expect_equal(k$df_btw, 2)
  expect_equal(k$df_within, 29)
  expect_equal(k$df_total, 31)
  expect_equal(k$ms_btw, 412.392)
  expect_equal(k$ms_within, 10.388)
  expect_equal(k$f, 39.699)
  expect_equal(k$p, 0)
  expect_equal(k$r2, 0.7325)
  expect_equal(k$ar2, 0.714)
  expect_equal(k$sigma, 3.2231)
  expect_equal(k$obs, 32)
})

test_that("output from one way anova is as expected", {
  x <- cat("                               ANOVA
--------------------------------------------------------------------
                   Sum of
                  Squares     DF    Mean Square      F        Sig.
--------------------------------------------------------------------
Between Groups    824.785     2       412.392      39.699    0.0000
Within Groups     301.263     29      10.388
Total             1126.048    31
--------------------------------------------------------------------
                 Report
----------------------------------------
 Category     N      Mean     Std. Dev.
----------------------------------------
    4         11    26.664        4.510
    6         7     19.743        1.454
    8         14    15.100        2.560
----------------------------------------
Number of obs = 32        R-squared     = 0.7325
Root MSE      = 3.2231    Adj R-squared = 0.714")

  expect_equivalent(print(infer_oneway_anova(mtcars, mpg, cyl)), x)
})