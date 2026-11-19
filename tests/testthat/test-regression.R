# Ordinary valid inputs must be unchanged by the input-validation work in
# #99. The expected numbers below were captured from the package before the
# validation helper was introduced. The Satterthwaite degrees of freedom are
# excluded here because #89 intentionally stores them unrounded; that change is
# covered in test-indttest.R.

test_that("ordinary ANOVA results are unchanged (#99 regression)", {
  k <- ifr_oneway_anova(mtcars, mpg, cyl)

  expect_equal(k$fstat, 39.698)
  expect_equal(k$r2, 0.7325)
  expect_equal(k$rmse, 3.2231)
  expect_equal(k$pval, 4.97891916584e-09)
})

test_that("ordinary one-sample variance results are unchanged (#99 regression)", {
  k <- ifr_os_var_test(mtcars, mpg, 5)

  expect_equal(k$chi, 45.0419)
  expect_equal(k$c_lwr, 4.8318)
  expect_equal(k$c_upr, 8.0127)
  expect_equal(k$se, 1.0654)
  expect_equal(k$xbar, 20.0906)
})

test_that("ordinary independent t test results are unchanged (#99 regression)", {
  k <- ifr_ts_ind_ttest(hsb, female, write)

  expect_equal(k$t_pooled, -3.7341)
  expect_equal(k$t_satterthwaite, -3.6564)
  expect_equal(k$conf_diff, c(-7.44183, -2.29806))
  expect_equal(k$conf_diff_satterthwaite, c(-7.49916, -2.24073))
  expect_equal(k$sig_pooled, 2e-04)
  expect_equal(k$sig, 3e-04)
})

test_that("ordinary paired t test results are unchanged (#99 regression)", {
  k <- ifr_ts_paired_ttest(hsb, read, write)

  expect_equal(k$tstat, -0.8673)
  expect_equal(k$conf_int_diff, c(-1.784142402, 0.694142402), tolerance = 1e-8)
  expect_equal(k$corsig, 0)
})

test_that("ordinary proportion calculator results are unchanged (#99 regression)", {
  k <- ifr_ts_prop_calc(30, 25, 0.3, 0.5)

  expect_equal(k$z, -1.514)
  expect_equal(k$sig, 0.13)
  expect_equal(k$phat1, 0.3)
  expect_equal(k$phat2, 0.5)
})

test_that("ordinary one-sample proportion results are unchanged (#99 regression)", {
  k <- ifr_os_prop_test(200, phat = 0.3, prob = 0.5)

  expect_equal(k$z, -5.6569)
  expect_equal(k$sig, 0)
  expect_equal(k$obs, c(140, 60))
})

test_that("ordinary binomial results are unchanged (#99 regression)", {
  k <- ifr_binom_calc(32, 13, prob = 0.5)

  expect_equal(k$pval_lower, 0.188543)
  expect_equal(k$pval_upper, 0.892336)
  expect_equal(k$two_tail, 0.377085587, tolerance = 1e-7)
})

test_that("ordinary goodness-of-fit results are unchanged (#99 regression)", {
  k <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))

  expect_equal(k$chisquare, 5.0286)
  expect_equal(k$pvalue, 0.1697)
  expect_equal(k$observed_frequency, c(24, 11, 20, 145))
})
