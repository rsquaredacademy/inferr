test_that("ouput from ifr_chisq_assoc_test matches the expected result", {
  k <- ifr_chisq_assoc_test(hsb, female, schtyp)
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

test_that("ouput from ifr_chisq_assoc_test matches the expected result", {
  k <- ifr_chisq_assoc_test(hsb, female, ses)
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

test_that("ifr_chisq_assoc_test throws the appropriate error", {
  expect_error(ifr_chisq_assoc_test(hsb, female, read), "y must be a categorical variable")
  expect_error(ifr_chisq_assoc_test(hsb, read, ses), "x must be a categorical variable")
})

test_that("ouput from ifr_chisq_assoc_test is as expected", {
  expect_snapshot(ifr_chisq_assoc_test(hsb, female, schtyp))
})

test_that("ouput from ifr_chisq_assoc_test 2 is as expected", {
  expect_snapshot(ifr_chisq_assoc_test(hsb, female, ses))
})
