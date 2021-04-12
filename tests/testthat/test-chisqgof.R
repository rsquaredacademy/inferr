context("chi2-gof")

test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
  expect_equal(k$chisquare, 5.0286)
  expect_equal(k$pvalue, 0.1697)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equivalent(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equivalent(k$observed_frequency, c(24, 11, 20, 145))
  expect_equivalent(k$expected_frequency, c(20, 20, 20, 140))
  expect_equivalent(k$deviation, c(" 20.00", "-45.00", "  0.00", "  3.57"))
  expect_equivalent(k$std_residuals, c(" 0.89", "-2.01", " 0.00", " 0.42"))
  expect_equivalent(k$varname, "race")
})


test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(0.1, 0.1, 0.1, 0.7))
  expect_equal(k$chisquare, 5.0286)
  expect_equal(k$pvalue, 0.1697)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equivalent(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equivalent(k$observed_frequency, c(24, 11, 20, 145))
  expect_equivalent(k$expected_frequency, c(20, 20, 20, 140))
  expect_equivalent(k$deviation, c(" 20.00", "-45.00", "  0.00", "  3.57"))
  expect_equivalent(k$std_residuals, c(" 0.89", "-2.01", " 0.00", " 0.42"))
  expect_equivalent(k$varname, "race")
})

test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
  expect_equal(k$chisquare, 4.3821)
  expect_equal(k$pvalue, 0.2231)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equivalent(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equivalent(k$observed_frequency, c(24, 11, 20, 145))
  expect_equivalent(k$expected_frequency, c(20, 20, 20, 140))
  expect_equivalent(k$deviation, c(" 17.50", "-47.50", " -2.50", "  3.21"))
  expect_equivalent(k$std_residuals, c(" 0.78", "-2.12", "-0.11", " 0.38"))
  expect_equivalent(k$varname, "race")
})


test_that("ifr_chisq_gof_test throws appropriate errors", {
  expect_error(
    ifr_chisq_gof_test(
      hsb, race, c(20, 20, 20, 140),
      correct = "FALSE"
    ),
    "correct must be either TRUE or FALSE"
  )
  expect_error(
    ifr_chisq_gof_test(
      hsb, race,
      c("20", "20", "20", "140")
    ),
    "y must be numeric"
  )
  expect_error(
    ifr_chisq_gof_test(hsb, race, c(20, 20, 20)),
    "Length of y must be equal to the number of categories in x"
  )
})


test_that("output from ifr_chisq_gof_test is as expected", {
  x <- cat("    Test Statistics
-----------------------
Chi-Square       5.0286
DF                    3
Pr > Chi Sq      0.1697
Sample Size         200

                         Variable: race
-----------------------------------------------------------------
Category    Observed    Expected    % Deviation    Std. Residuals
-----------------------------------------------------------------
   1           24          20          20.00            0.89
   2           11          20         -45.00           -2.01
   3           20          20           0.00            0.00
   4          145         140           3.57            0.42
-----------------------------------------------------------------")

  expect_equivalent(print(ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))), x)
})
