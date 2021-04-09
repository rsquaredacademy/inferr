context("infer_ts_ind_ttest")

test_that("output from infer_ts_ind_ttest matches expected result", {
  k <- infer_ts_ind_ttest(hsb, female, write)
  expect_equivalent(k$levels, c("0", "1"))
  expect_equivalent(as.numeric(k$obs), c(91, 109))
  expect_equal(k$n, 200)
  expect_equivalent(as.numeric(k$mean), c(50.121, 54.991))
  expect_equivalent(as.numeric(k$sd), c(10.305, 8.134))
  expect_equivalent(as.numeric(k$se), c(1.080, 0.779))
  expect_equivalent(round(as.numeric(k$lower), 3), c(47.975, 53.447))
  expect_equivalent(round(as.numeric(k$upper), 3), c(52.267, 56.535))
  expect_equivalent(round(unlist(k$combined), 3), c(
    200.000, 52.775, 9.479, 0.670,
    199.000, 1.972, 51.454, 54.096
  ))
  expect_equal(k$mean_diff, -4.87)
  expect_equal(k$sd_dif, 9.231)
  expect_equal(k$se_dif, 1.304)
  expect_equivalent(round(k$conf_diff, 3), c(-7.426, -2.314))
  expect_equal(k$df_pooled, 198)
  expect_equal(k$df_satterthwaite, 170)
  expect_equal(round(k$t_pooled, 2), -3.73)
  expect_equal(round(k$t_satterthwaite, 2), -3.66)
  expect_equal(k$sig_pooled_l, 1e-04)
  expect_equal(k$sig_pooled_u, 0.9999)
  expect_equal(k$sig_pooled, 2e-04)
  expect_equal(k$sig, 3e-04)
  expect_equal(k$sig_l, 2e-04)
  expect_equal(k$sig_u, 0.9998)
  expect_equal(k$num_df, 90)
  expect_equal(k$den_df, 108)
  expect_equal(k$f, 1.605)
  expect_equal(k$f_sig, 0.0188)
  expect_equal(k$confint, 0.95)
  expect_equivalent(k$var_y, "write")
  expect_equivalent(k$alternative, "both")
})


test_that("output from independent sample t test is as expected when alternative is less", {
  x <- cat("                              Group Statistics
-----------------------------------------------------------------------------
  Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
-----------------------------------------------------------------------------
    0          91    50.121      1.080       10.305        47.975     52.267
    1         109    54.991      0.779        8.134        53.447     56.535
-----------------------------------------------------------------------------
 combined     200    52.775      0.67         9.479        51.454     54.096
-----------------------------------------------------------------------------
   diff       200    -4.87       1.304        9.231        -7.426     -2.314
-----------------------------------------------------------------------------
                      Independent Samples Test
                      ------------------------
                  Ho: mean(0) - mean(1) = diff = 0
                            Ha: diff < 0
---------------------------------------------------------------------
 Variable        Method        Variances    DF     t Value     P < t
---------------------------------------------------------------------
  write          Pooled          Equal      198    -3.7347    0.0001
  write       Satterthwaite     Unequal     170    -3.6564    0.0002
---------------------------------------------------------------------
                Test for Equality of Variances
---------------------------------------------------------------
 Variable      Method     Num DF    Den DF    F Value    P > F
---------------------------------------------------------------
  write       Folded F      90       108       1.605     0.0188
---------------------------------------------------------------")

  expect_equivalent(print(infer_ts_ind_ttest(hsb, female, write, alternative = "less")), x)
})

test_that("output from independent sample t test is as expected when alternative is greater", {
  x <- cat("                              Group Statistics
-----------------------------------------------------------------------------
  Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
-----------------------------------------------------------------------------
    0          91    50.121      1.080       10.305        47.975     52.267
    1         109    54.991      0.779        8.134        53.447     56.535
-----------------------------------------------------------------------------
 combined     200    52.775      0.67         9.479        51.454     54.096
-----------------------------------------------------------------------------
   diff       200    -4.87       1.304        9.231        -7.426     -2.314
-----------------------------------------------------------------------------
                      Independent Samples Test
                      ------------------------
                  Ho: mean(0) - mean(1) = diff = 0
                            Ha: diff > 0
---------------------------------------------------------------------
 Variable        Method        Variances    DF     t Value     P > t
---------------------------------------------------------------------
  write          Pooled          Equal      198    -3.7347    0.9999
  write       Satterthwaite     Unequal     170    -3.6564    0.9998
---------------------------------------------------------------------
                Test for Equality of Variances
---------------------------------------------------------------
 Variable      Method     Num DF    Den DF    F Value    P > F
---------------------------------------------------------------
  write       Folded F      90       108       1.605     0.0188
---------------------------------------------------------------")

  expect_equivalent(print(infer_ts_ind_ttest(hsb, female, write, alternative = "greater")), x)
})

test_that("output from independent sample t test is as expected when alternative is both", {
  x <- cat("                              Group Statistics
-----------------------------------------------------------------------------
  Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
-----------------------------------------------------------------------------
    0          91    50.121      1.080       10.305        47.975     52.267
    1         109    54.991      0.779        8.134        53.447     56.535
-----------------------------------------------------------------------------
 combined     200    52.775      0.67         9.479        51.454     54.096
-----------------------------------------------------------------------------
   diff       200    -4.87       1.304        9.231        -7.426     -2.314
-----------------------------------------------------------------------------
                      Independent Samples Test
                      ------------------------
                  Ho: mean(0) - mean(1) = diff = 0
                            Ha: diff ~= 0
---------------------------------------------------------------------
 Variable        Method        Variances    DF     t Value    P > |t|
---------------------------------------------------------------------
  write          Pooled          Equal      198    -3.7347    0.0002
  write       Satterthwaite     Unequal     170    -3.6564    0.0003
---------------------------------------------------------------------
                Test for Equality of Variances
---------------------------------------------------------------
 Variable      Method     Num DF    Den DF    F Value    P > F
---------------------------------------------------------------
  write       Folded F      90       108       1.605     0.0188
---------------------------------------------------------------")

  expect_equivalent(print(infer_ts_ind_ttest(hsb, female, write, alternative = "both")), x)
})

test_that("output from independent sample t test is as expected when alternative is all", {
  x <- cat("                              Group Statistics
-----------------------------------------------------------------------------
  Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
-----------------------------------------------------------------------------
    0          91    50.121      1.080       10.305        47.975     52.267
    1         109    54.991      0.779        8.134        53.447     56.535
-----------------------------------------------------------------------------
 combined     200    52.775      0.67         9.479        51.454     54.096
-----------------------------------------------------------------------------
   diff       200    -4.87       1.304        9.231        -7.426     -2.314
-----------------------------------------------------------------------------
                        Independent Samples Test
                      ------------------------
                    Ho: mean(0) - mean(1) = diff = 0
      Ha: diff < 0            Ha: diff ~= 0             Ha: diff > 0
                                  Pooled
------------------------------------------------------------------------
       t = -3.7347              t = -3.7347              t = -3.7347
     P < t = 0.0001          P > |t| = 0.0002          P > t = 0.9999
                              Satterthwaite
------------------------------------------------------------------------
       t = -3.6564              t = -3.6564              t = -3.6564
     P < t = 0.0002          P > |t| = 0.0003          P > t = 0.9998
                Test for Equality of Variances
---------------------------------------------------------------
 Variable      Method     Num DF    Den DF    F Value    P > F
------------------------------------------------
  write       Folded F      90       108       1.605     0.0188
---------------------------------------------------
------------")

  expect_equivalent(print(infer_ts_ind_ttest(hsb, female, write, alternative = "all")), x)
})

