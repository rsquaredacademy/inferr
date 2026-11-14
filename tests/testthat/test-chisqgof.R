test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
  expect_equal(k$chisquare, 5.0286)
  expect_equal(k$pvalue, 0.1697)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equal(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equal(k$observed_frequency, c(24, 11, 20, 145))
  expect_equal(k$expected_frequency, c(20, 20, 20, 140))
  expect_equal(k$deviation, c(" 20.00", "-45.00", "  0.00", "  3.57"))
  expect_equal(k$std_residuals, c(" 0.89", "-2.01", " 0.00", " 0.42"))
  expect_equal(k$varname, "race")
})


test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(0.1, 0.1, 0.1, 0.7))
  expect_equal(k$chisquare, 5.0286)
  expect_equal(k$pvalue, 0.1697)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equal(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equal(k$observed_frequency, c(24, 11, 20, 145))
  expect_equal(k$expected_frequency, c(20, 20, 20, 140))
  expect_equal(k$deviation, c(" 20.00", "-45.00", "  0.00", "  3.57"))
  expect_equal(k$std_residuals, c(" 0.89", "-2.01", " 0.00", " 0.42"))
  expect_equal(k$varname, "race")
})

test_that("output from ifr_chisq_gof_test matches the expected output", {
  k <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
  expect_equal(k$chisquare, 4.3696)
  expect_equal(k$pvalue, 0.2242)
  expect_equal(k$degrees_of_freedom, 3)
  expect_equal(k$sample_size, 200)
  expect_equal(k$categories, c("1", "2", "3", "4"))
  expect_equal(k$n_levels, 4)
  expect_equal(k$observed_frequency, c(24, 11, 20, 145))
  expect_equal(k$expected_frequency, c(20, 20, 20, 140))
  expect_equal(k$deviation, c(" 17.50", "-42.50", "  0.00", "  3.21"))
  expect_equal(k$std_residuals, c(" 0.78", "-1.90", " 0.00", " 0.38"))
  expect_equal(k$varname, "race")
})


test_that("ifr_chisq_gof_test ignores missing observations (H1)", {
  d <- data.frame(x = factor(c("no", "yes", "yes", NA),
                             levels = c("no", "yes")))
  k   <- ifr_chisq_gof_test(d, x, c(.5, .5))
  ref <- suppressWarnings(chisq.test(c(1, 2), p = c(.5, .5)))

  expect_equal(k$sample_size, 3)
  expect_equal(k$observed_frequency, c(1, 2))
  expect_equal(k$expected_frequency, c(1.5, 1.5))
  expect_equal(k$chisquare, round(unname(ref$statistic), 4))
  expect_equal(k$pvalue, round(ref$p.value, 4))

  # nearby case: proportions supplied instead of counts
  k2 <- ifr_chisq_gof_test(d, x, c(.8, .2))
  expect_equal(k2$sample_size, 3)
  expect_equal(k2$expected_frequency, c(2.4, 0.6))
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

test_that("Yates correction is applied only when requested", {
  d <- data.frame(x = factor(c(rep("a", 30), rep("b", 70))))

  k <- ifr_chisq_gof_test(d, x, c(50, 50), correct = FALSE)
  expect_equal(k$chisquare, as.numeric(chisq.test(c(30, 70))$statistic))
  expect_equal(k$chisquare, 16)

  k <- ifr_chisq_gof_test(d, x, c(50, 50), correct = TRUE)
  expect_equal(k$chisquare, 15.21)
})


test_that("output from ifr_chisq_gof_test is as expected", {
  expect_snapshot(ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140)))
})

test_that("GOF continuity correction is bounded at the observed deviation (H3)", {
  d <- data.frame(x = factor(rep(c("a", "b"), each = 10)))
  k <- ifr_chisq_gof_test(d, x, c(10, 10), correct = TRUE)

  expect_equal(k$chisquare, 0)
  expect_equal(k$pvalue, 1)
  expect_equal(as.vector(k$deviation), c("0.00", "0.00"))
  expect_equal(as.vector(k$std_residuals), c("0.00", "0.00"))

  # nearby case: corrected residuals follow the signed bounded deviation
  d2  <- data.frame(x = factor(c(rep("a", 30), rep("b", 70))))
  k2  <- ifr_chisq_gof_test(d2, x, c(50, 50), correct = TRUE)
  obs <- c(30, 70)
  exp <- c(50, 50)
  corr_dev <- sign(obs - exp) * pmax(abs(obs - exp) - 0.5, 0)

  expect_equal(k2$chisquare, sum(corr_dev ^ 2 / exp))
  expect_equal(k2$chisquare, 15.21)
  expect_equal(as.vector(k2$deviation), format(round(corr_dev / exp * 100, 2), nsmall = 2))
  expect_equal(as.vector(k2$std_residuals), format(round(corr_dev / sqrt(exp), 2), nsmall = 2))
})
