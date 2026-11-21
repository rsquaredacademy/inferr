# Value tests for the rx25 audit findings (H1-H4, M1-M7, L1).

test_that("H2: proportion calculator enforces the sample-size domain", {
  expect_error(
    ifr_ts_prop_calc(1e308, 1e308, .3, .5),
    "n1 must be a positive integer no greater than 1e15"
  )
  expect_error(
    ifr_ts_prop_calc(1e15 + 1, 10, .3, .5),
    "n1 must be a positive integer no greater than 1e15"
  )
  expect_error(
    ifr_ts_prop_calc(10, 1e15 + 1, .3, .5),
    "n2 must be a positive integer no greater than 1e15"
  )

  # inside the domain the z statistic follows the pooled formula
  k     <- ifr_ts_prop_calc(1e12, 1e12, .3, .5)
  phat  <- .4
  z     <- (.3 - .5) / sqrt(phat * (1 - phat) * (1 / 1e12 + 1 / 1e12))
  expect_equal(k$z, round(z, 3))
  expect_equal(k$n1, 1e12)
  expect_equal(k$n2, 1e12)

  # scaling both sample sizes by four doubles the z statistic
  z1 <- ifr_ts_prop_calc(1000, 1000, .3, .5)$z
  z2 <- ifr_ts_prop_calc(4000, 4000, .3, .5)$z
  expect_equal(z2, 2 * z1, tolerance = 1e-3)

  # an underflowed tail still prints as a bound, not as an exact zero
  out <- capture.output(print(k))
  expect_true(any(grepl("< 0.001", out, fixed = TRUE)))
})

test_that("H3: the three proportion interfaces print p-value bounds", {
  calc <- ifr_ts_prop_calc(30, 25, 0, 1)
  expect_equal(calc$sig, 0)
  expect_gt(calc$sig_raw, 0)
  z <- -1 / sqrt((25 / 55) * (30 / 55) * (1 / 30 + 1 / 25))
  expect_equal(calc$sig_raw, 2 * pnorm(abs(z), lower.tail = FALSE),
               tolerance = 1e-12)
  expect_true(any(grepl("< 0.001", capture.output(print(calc)),
                        fixed = TRUE)))

  one <- ifr_os_prop_test(200, phat = .3, prob = .5)
  expect_equal(one$sig, 0)
  expect_gt(one$sig_raw, 0)
  expect_true(any(grepl("< 0.0001", capture.output(print(one)),
                        fixed = TRUE)))

  d <- data.frame(
    a = factor(c(rep("no", 30), rep(NA, 25)), levels = c("no", "yes")),
    b = factor(c(rep(NA, 30), rep("yes", 25)), levels = c("no", "yes"))
  )
  two <- ifr_ts_prop_test(d, a, b)
  expect_equal(two$sig, 0)
  expect_gt(two$sig_raw, 0)
  expect_true(any(grepl("< 0.001", capture.output(print(two)),
                        fixed = TRUE)))

  grouped <- data.frame(
    out = factor(c(rep("no", 30), rep("yes", 25)), levels = c("no", "yes")),
    grp = factor(rep(c("a", "b"), c(30, 25)))
  )
  grp <- ifr_ts_prop_group(grouped, out, grp)
  expect_equal(grp$sig, 0)
  expect_gt(grp$sig_raw, 0)
  expect_true(any(grepl("< 0.001", capture.output(print(grp)),
                        fixed = TRUE)))
})

test_that("H3: other rounding printers print p-value bounds", {
  gof_data <- data.frame(x = factor(rep("a", 200), levels = c("a", "b")))
  gof      <- ifr_chisq_gof_test(gof_data, x, c(100, 100))
  expect_equal(gof$pvalue, 0)
  expect_gt(gof$pvalue_raw, 0)
  expect_true(any(grepl("< 0.0001", capture.output(print(gof)),
                        fixed = TRUE)))

  mc <- ifr_mcnemar_test(matrix(c(0, 100, 0, 0), 2))
  expect_equal(mc$pvalue, 0)
  expect_gt(mc$pvalue_raw, 0)
  expect_true(any(grepl("< 0.0001", capture.output(print(mc)),
                        fixed = TRUE)))

  tt <- ifr_os_t_test(hsb, write, mu = 0)
  expect_equal(tt$p, 0)
  expect_true(any(grepl("< 0.00001", capture.output(print(tt)),
                        fixed = TRUE)))

  paired <- ifr_ts_paired_ttest(
    data.frame(a = 1:10,
               b = 101:110 + rep(c(-1, 1), 5)),
    a, b
  )
  expect_equal(paired$p_two_tail, 0)
  expect_true(any(grepl("< 0.001", capture.output(print(paired)),
                        fixed = TRUE)))

  ind <- ifr_ts_ind_ttest(
    data.frame(g = factor(rep(c("a", "b"), each = 10)),
               y = c(1:10, 101:110)),
    g, y
  )
  expect_equal(ind$sig, 0)
  expect_true(any(grepl("< 0.0001", capture.output(print(ind)),
                        fixed = TRUE)))

  var <- ifr_os_var_test(data.frame(x = 1:10), x, .1)
  expect_equal(var$p_two, 0)
  expect_true(any(grepl("< 0.0001", capture.output(print(var)),
                        fixed = TRUE)))
})

test_that("H4: variance and paired inputs outside the domain stop clearly", {
  d <- data.frame(x = (1:4) * 1e160, y = (4:1) * 1e160)

  expect_error(
    ifr_os_var_test(d, x, 1),
    "x must contain only finite values with absolute value <= 1e150"
  )
  expect_error(
    ifr_ts_paired_ttest(d, x, y),
    "x must contain only finite values with absolute value <= 1e150"
  )
  expect_error(
    ifr_ts_paired_ttest(data.frame(x = 1:4, y = (1:4) * 1e160), x, y),
    "y must contain only finite values with absolute value <= 1e150"
  )
})

test_that("M1: ANOVA's relative guard works at any finite response scale", {
  d <- data.frame(x = (1:6) * 1e-170,
                  g = factor(rep(c("a", "b"), each = 3)))
  k <- ifr_oneway_anova(d, x, g)

  expect_equal(k$fstat, 13.5)
  expect_equal(round(k$pval, 8), 0.02131164)

  # the same design at an out-of-domain scale is rejected by the range guard
  d$x <- (1:6) * 1e160
  expect_error(
    ifr_oneway_anova(d, x, g),
    "x must contain only finite values with absolute value <= 1e150"
  )

  # constants are still rejected at every scale
  for (value in c(0, 1, 1e-170)) {
    d$x <- rep(value, 6)
    expect_error(ifr_oneway_anova(d, x, g), "response 'x' has no variation")
  }
})

test_that("M2: GOF expected counts must total the sample size", {
  d <- data.frame(x = factor(c("a", "a", "b", "b")))

  expect_error(
    ifr_chisq_gof_test(d, x, c(10, 10)),
    "expected counts in y must sum to the sample size \\(4\\)"
  )

  # proportions (sum one) and counts (sum n) both work
  kp <- ifr_chisq_gof_test(d, x, c(.5, .5))
  kc <- ifr_chisq_gof_test(d, x, c(2, 2))
  expect_equal(kp$chisquare, 0)
  expect_equal(kp$pvalue, 1)
  expect_equal(kp$chisquare, kc$chisquare)
  expect_equal(kp$expected_frequency, c(2, 2))
})

test_that("M3: GOF drops an explicit NA factor level with a message", {
  d <- data.frame(x = addNA(factor(c("a", "b", NA))))

  expect_message(
    k <- ifr_chisq_gof_test(d, x, c(1, 1)),
    "removing 1 observation"
  )

  expect_equal(k$sample_size, 2)
  expect_equal(k$categories, c("a", "b"))
  expect_equal(k$observed_frequency, c(1, 1))
  expect_silent(capture.output(print(k)))
})

test_that("M4 and M7: McNemar checks shape, integer counts and empty tables", {
  expect_error(
    ifr_mcnemar_test(matrix(1:4, 1, 4)),
    "data must be a 2 x 2 matrix"
  )
  expect_error(
    ifr_mcnemar_test(matrix(c(2, .1, 3, 2), 2)),
    "data must contain non-negative integer counts"
  )
  expect_error(
    ifr_mcnemar_test(matrix(0, 2, 2)),
    "data must contain at least one observation"
  )

  # a valid table still returns the base-R statistic
  m <- matrix(c(10, 2, 1, 5), 2)
  k <- ifr_mcnemar_test(m)
  expect_equal(k$statistic,
               round(unname(mcnemar.test(m, correct = FALSE)$statistic), 4))
})

test_that("M5: variance and paired tests require usable samples", {
  expect_error(
    ifr_os_var_test(data.frame(x = 1), x, 1),
    "x must contain at least two non-missing observations"
  )
  expect_error(
    ifr_ts_paired_ttest(data.frame(x = 1, y = 2), x, y),
    "x and y must contain at least two complete pairs"
  )
  expect_error(
    ifr_ts_paired_ttest(data.frame(x = numeric(0), y = numeric(0)), x, y),
    "x and y must contain at least two complete pairs"
  )

  # two complete pairs remain valid, but correlation significance is undefined
  k <- ifr_ts_paired_ttest(data.frame(x = c(1, 2), y = c(2, 1)), x, y)
  expect_equal(k$Obs, 2)
  expect_true(is.na(k$corsig))
})

test_that("M6: empty two-sample proportions name the empty sample", {
  d <- data.frame(a = factor(levels = 0:1), b = factor(levels = 0:1))
  expect_error(
    ifr_ts_prop_test(d, a, b),
    "var1 must contain at least one non-missing observation"
  )

  d2 <- data.frame(
    a = factor(c("no", "yes"), levels = c("no", "yes")),
    b = factor(c(NA, NA), levels = c("no", "yes"))
  )
  expect_error(
    ifr_ts_prop_test(d2, a, b),
    "var2 must contain at least one non-missing observation"
  )
})

test_that("L1: touched help pages state the supported input domain", {
  pages <- c(
    "ifr_oneway_anova.Rd", "ifr_chisq_gof_test.Rd",
    "ifr_ts_prop_test.Rd", "ifr_os_prop_test.Rd",
    "ifr_os_t_test.Rd", "ifr_os_var_test.Rd",
    "ifr_ts_ind_ttest.Rd", "ifr_ts_paired_ttest.Rd",
    "ifr_mcnemar_test.Rd"
  )

  for (page in pages) {
    rd <- test_path("..", "..", "man", page)
    skip_if_not(file.exists(rd))
    txt <- paste(readLines(rd), collapse = "\n")
    expect_match(txt, "Supported inputs", fixed = TRUE)
    expect_match(txt, "1e150", fixed = TRUE)
    expect_match(txt, "1e15", fixed = TRUE)
  }
})
