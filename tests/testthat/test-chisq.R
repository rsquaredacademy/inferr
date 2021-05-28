context("chi2-contingency")

test_that("ouput from infer_chisq_assoc_test matches the expected result", {
  k <- infer_chisq_assoc_test(hsb, female, schtyp)
  expect_equal(k$df, 1)
  expect_equal(k$chisquare, 0.047)
  expect_equal(k$chisquare_lr, 0.0471)
  expect_equal(k$chisquare_mantel_haenszel, 0.0468)
  expect_equal(k$chisquare_adjusted, 0.0005)
  expect_equal(k$pval_chisquare, 0.8284)
  expect_equal(k$pval_chisquare_lr, 0.8282)
  expect_equal(k$pval_chisquare_mantel_haenszel, 0.8287)
  expect_equal(k$pval_chisquare_adjusted, 0.9822)
  expect_equal(k$phi_coefficient, 0.0153)
  expect_equal(k$contingency_coefficient, 0.0153)
  expect_equal(k$cramers_v, 0.0153)
  expect_equal(k$ds, 4)
})

test_that("ouput from infer_chisq_assoc_test matches the expected result", {
  k <- infer_chisq_assoc_test(hsb, female, ses)
  expect_equal(k$df, 2)
  expect_equal(k$chisquare, 4.5765)
  expect_equal(k$chisquare_lr, 4.6789)
  expect_equal(k$pval_chisquare, 0.1014)
  expect_equal(k$pval_chisquare_lr, 0.0964)
  expect_equal(k$phi_coefficient, 0.1513)
  expect_equal(k$contingency_coefficient, 0.1496)
  expect_equal(k$cramers_v, 0.1513)
  expect_equal(k$ds, 6)
})

test_that("infer_chisq_assoc_test throws the appropriate error", {
  expect_error(infer_chisq_assoc_test(hsb, female, read), "y must be a categorical variable")
  expect_error(infer_chisq_assoc_test(hsb, read, ses), "x must be a categorical variable")
})

test_that("ouput from infer_chisq_assoc_test is as expected", {
  x <- cat("               Chi Square Statistics

Statistics                     DF    Value      Prob
----------------------------------------------------
Chi-Square                     1    0.0470    0.8284
Likelihood Ratio Chi-Square    1    0.0471    0.8282
Continuity Adj. Chi-Square     1    0.0005    0.9822
Mantel-Haenszel Chi-Square     1    0.0468    0.8287
Phi Coefficient                     0.0153
Contingency Coefficient             0.0153
Cramer's V                          0.0153
----------------------------------------------------")

  expect_equivalent(print(infer_chisq_assoc_test(hsb, female, schtyp)), x)
})

test_that("ouput from infer_chisq_assoc_test 2 is as expected", {
  x <- cat("               Chi Square Statistics

Statistics                     DF    Value      Prob
----------------------------------------------------
Chi-Square                     2    4.5765    0.1014
Likelihood Ratio Chi-Square    2    4.6789    0.0964
Phi Coefficient                     0.1513
Contingency Coefficient             0.1496
Cramer's V                          0.1513
----------------------------------------------------")

  expect_equivalent(print(infer_chisq_assoc_test(hsb, female, ses)), x)
})
