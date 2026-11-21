# Value tests for the rx25b re-audit findings (H1-H8, M1-M2, L1).

test_that("H1: GOF keeps an unused declared level and restores the 5722a08 result", {
  d <- data.frame(x = factor(c("a", "b", NA), levels = c("a", "b", "c")))

  expect_message(
    k <- ifr_chisq_gof_test(d, x, c(1, 1, 1)),
    "removing 1 observation"
  )

  # the same statistic, degrees of freedom and p-value as develop 5722a08
  expect_equal(k$chisquare, 1)
  expect_equal(k$degrees_of_freedom, 2)
  expect_equal(k$pvalue, 0.6065)
  expect_equal(k$pvalue_raw, pchisq(1, 2, lower.tail = FALSE),
               tolerance = 1e-12)
  expect_equal(k$categories, c("a", "b", "c"))
  expect_equal(k$observed_frequency, c(1, 1, 0))

  # c(1, 1, 1) is the equal-proportions shorthand; expected counts are n / 3
  expect_equal(k$expected_frequency, rep(2 / 3, 3), tolerance = 1e-12)
  ref <- suppressWarnings(chisq.test(c(1, 1, 0), p = rep(1 / 3, 3)))
  expect_equal(k$chisquare, round(unname(ref$statistic), 4))
  expect_equal(k$pvalue, round(ref$p.value, 4))
})

test_that("H1: GOF retains an unused non-NA level without missing values", {
  d <- data.frame(x = factor(c("a", "b", "a", "b"),
                             levels = c("a", "b", "c")))
  k <- ifr_chisq_gof_test(d, x, c(1 / 3, 1 / 3, 1 / 3))
  ref <- suppressWarnings(chisq.test(c(2, 2, 0), p = rep(1 / 3, 3)))

  expect_equal(k$categories, c("a", "b", "c"))
  expect_equal(k$observed_frequency, c(2, 2, 0))
  expect_equal(k$degrees_of_freedom, 2)
  expect_equal(k$chisquare, round(unname(ref$statistic), 4))
  expect_equal(k$pvalue, round(ref$p.value, 4))
})

test_that("H1: GOF drops an explicit NA level but keeps the others", {
  d <- data.frame(x = factor(c("a", "b"), levels = c("a", "b", NA),
                             exclude = NULL))

  # the audit's three-expectation call is a clear length error, not a crash
  expect_error(
    ifr_chisq_gof_test(d, x, c(.4, .4, .2)),
    "Length of y must be equal to the number of categories"
  )

  k <- ifr_chisq_gof_test(d, x, c(.5, .5))
  expect_equal(k$categories, c("a", "b"))
  expect_equal(k$sample_size, 2)
  expect_equal(k$expected_frequency, c(1, 1))
  expect_silent(capture.output(print(k)))
})

test_that("H2: paired t test computes with a constant control column", {
  d <- data.frame(x = c(1, 3, 4, 7), y = rep(2, 4))
  k <- ifr_ts_paired_ttest(d, x, y)
  ref <- t.test(d$x, d$y, paired = TRUE)

  expect_equal(k$tstat, round(unname(ref$statistic), 4))
  expect_equal(k$df, unname(ref$parameter))
  expect_equal(k$p_two_tail, ref$p.value, tolerance = 1e-12)
  expect_true(is.na(k$corr))
  expect_true(is.na(k$corsig))

  out <- capture.output(print(k))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("NA", out, fixed = TRUE)))
})

test_that("H2: paired t test prints a complete two-pair object", {
  k <- ifr_ts_paired_ttest(data.frame(x = c(1, 4), y = c(2, 3)), x, y)

  expect_equal(k$Obs, 2)
  expect_equal(k$tstat, 0)
  expect_equal(k$df, 1)
  expect_equal(k$p_two_tail, 1)

  out <- capture.output(print(k))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("1.000", out, fixed = TRUE)))
  expect_false(any(is.na(nchar(out))))
})

test_that("H3: ANOVA converts unrounded sums of squares back to original units", {
  d <- data.frame(x = 1:6 * 1e149, g = factor(rep(1:2, each = 3)))
  k <- ifr_oneway_anova(d, x, g)

  expect_equal(k$ss_between, 1.35e299, tolerance = 1e-12)
  expect_equal(k$ss_within, 4e298)
  expect_equal(k$ss_total, 1.75e299, tolerance = 1e-12)
  expect_equal(k$ms_btw, 1.35e299, tolerance = 1e-12)
  expect_equal(k$ms_within, 1e298)
  expect_equal(k$fstat, 13.5)
  expect_equal(k$pval, pf(13.5, 1, 4, lower.tail = FALSE),
               tolerance = 1e-12)

  out <- capture.output(print(k))
  expect_true(any(grepl("4e\\+298", out)))
})

test_that("H3: ANOVA is scale invariant at a representable tiny scale", {
  d <- data.frame(x = 1:6 * 1e-140, g = factor(rep(1:2, each = 3)))
  k <- ifr_oneway_anova(d, x, g)

  expect_equal(k$ss_between, 1.35e-279, tolerance = 1e-12)
  expect_equal(k$ss_within, 4e-280)
  expect_equal(k$ss_total, 1.75e-279, tolerance = 1e-12)
  expect_equal(k$ms_within, 1e-280)
  expect_equal(k$fstat, 13.5)
  expect_equal(k$pval, pf(13.5, 1, 4, lower.tail = FALSE),
               tolerance = 1e-12)

  # below the representable scale a clear error replaces a table of zeroes
  d$x <- 1:6 * 1e-170
  expect_error(
    ifr_oneway_anova(d, x, g),
    "sums of squares for response 'x' underflow"
  )
})

test_that("H4: one-sample variance test is scale invariant at tiny scales", {
  tiny <- ifr_os_var_test(data.frame(x = 1:4 * 1e-170), x, 1e-170)
  unit <- ifr_os_var_test(data.frame(x = 1:4), x, 1)

  expect_equal(tiny$chi, 5)
  expect_equal(tiny$chi, unit$chi)
  expect_equal(tiny$p_two, unit$p_two)
  expect_equal(tiny$p_two, 0.3435943, tolerance = 1e-7)
  expect_gt(tiny$chi_raw, 0)
})

test_that("H5: two-sample proportions stay finite near the smallest doubles", {
  sub <- ifr_ts_prop_calc(10, 10, 5e-324, 0, alternative = "all")
  expect_true(is.finite(sub$z_raw))
  expect_lt(abs(sub$z_raw), 1e-100)
  expect_equal(unname(sub$sig_raw[["both"]]), 1, tolerance = 1e-12)
  expect_equal(unname(sub$sig_raw[["less"]]), 0.5, tolerance = 1e-12)
  expect_equal(unname(sub$sig_raw[["greater"]]), 0.5, tolerance = 1e-12)

  near_one <- ifr_ts_prop_calc(100, 100, 1, 1 - 2^-53,
                               alternative = "all")
  expect_true(is.finite(near_one$z_raw))
  expect_lt(abs(near_one$z_raw - 1.0536712e-7), 1e-12)
  expect_gt(unname(near_one$sig_raw[["both"]]), 0.99)
})

test_that("H5: one-sample proportion stays finite and prints honest bounds", {
  k <- ifr_os_prop_test(10, prob = 5e-324, phat = .5,
                        alternative = "all")

  expect_true(is.finite(k$z_raw))
  expect_gt(k$z_raw, 1e100)
  expect_equal(unname(k$sig[["less"]]), 1)
  expect_equal(unname(k$sig[["two-both"]]), 0)
  expect_true(any(grepl("> 1e308", k$deviation, fixed = TRUE)))
  expect_false(any(grepl("Inf", k$deviation, fixed = TRUE)))

  out <- capture.output(print(k))
  expect_true(any(grepl("< 0.0001", out, fixed = TRUE)))
  expect_false(any(grepl("Inf", out, fixed = TRUE)))
})

test_that("H6: ANOVA prints an underflowed positive p-value as a bound", {
  d <- data.frame(x = c(rep(1:5, 100), rep(11:15, 100)),
                  g = rep(1:2, each = 500))
  k <- ifr_oneway_anova(d, x, g)

  expect_equal(k$fstat, 12475)
  expect_equal(k$pval, 0)
  ref_log <- pf(12475, 1, 998, lower.tail = FALSE, log.p = TRUE)
  expect_true(is.finite(ref_log))
  expect_lt(ref_log, log(0.0001))

  out <- capture.output(print(k))
  expect_true(any(grepl("< 0.0001", out, fixed = TRUE)))
  expect_false(any(grepl("0\\.0000", out)))
})

test_that("H6: independent-t variance row prints an underflowed p-value bound", {
  d <- data.frame(g = factor(rep(1:2, each = 30)),
                  x = c(rep(c(-100, 100), 15), rep(c(-1, 1), 15)))
  k <- ifr_ts_ind_ttest(d, g, x, alternative = "all")

  expect_equal(k$f_sig, 0)
  expect_gt(k$f_sig_raw, 0)
  expect_equal(k$f_sig_raw,
               2 * pf(k$f_raw, k$num_df, k$den_df, lower.tail = FALSE),
               tolerance = 1e-12)

  out <- capture.output(print(k))
  folded <- grep("Folded F", out, value = TRUE)
  expect_true(length(folded) == 1L)
  expect_match(folded, "< 0.0001", fixed = TRUE)
})

test_that("H6: variance printers use unrounded statistics for p bounds", {
  os <- ifr_os_var_test(
    data.frame(x = seq(0, 1e-4, length.out = 100)), x, sd = 100
  )
  expect_equal(os$chi, 0)
  expect_gt(os$chi_raw, 0)
  os_out <- capture.output(print(os))
  expect_true(any(grepl("< 0.0001", os_out, fixed = TRUE)))

  ts <- ifr_ts_var_test(
    data.frame(x = (1:101) * 1e-5, y = 1:101), x, y,
    alternative = "less"
  )
  expect_equal(ts$f, 0)
  expect_gt(ts$f_raw, 0)
  expect_equal(ts$lower, 0)
  ts_out <- capture.output(print(ts))
  expect_true(any(grepl("< 0.0001", ts_out, fixed = TRUE)))

  # an unrepresentable variance is a clear error, not a zero F statistic
  expect_error(
    ifr_ts_var_test(
      data.frame(x = (1:101) * 1e-170, y = 1:101), x, y,
      alternative = "less"
    ),
    "positive, finite variances"
  )
})

test_that("H7: independent-t two-sided p-values stay in [0, 1]", {
  d <- data.frame(g = factor(rep(1:2, each = 5)),
                  x = c(1:5, (1:5) + .00001))

  for (dat in list(d, transform(d, g = factor(rep(2:1, each = 5))))) {
    k <- ifr_ts_ind_ttest(dat, g, x)
    welch  <- t.test(x ~ g, dat)
    pooled <- t.test(x ~ g, dat, var.equal = TRUE)

    expect_lte(k$sig_raw, 1)
    expect_gte(k$sig_raw, 0)
    expect_lte(k$sig_pooled_raw, 1)
    expect_gte(k$sig_pooled_raw, 0)
    expect_equal(k$sig_raw, welch$p.value, tolerance = 1e-12)
    expect_equal(k$sig_pooled_raw, pooled$p.value, tolerance = 1e-12)
  }

  d2 <- data.frame(g = factor(c("a", "a", "b", "b")),
                   x = c(0, 1, .00008, 1.00008))
  k2 <- ifr_ts_ind_ttest(d2, g, x)
  expect_lte(k2$sig_raw, 1)
  expect_equal(k2$sig_raw, t.test(x ~ g, d2)$p.value, tolerance = 1e-12)
  expect_equal(k2$sig_pooled_raw,
               t.test(x ~ g, d2, var.equal = TRUE)$p.value,
               tolerance = 1e-12)
})

test_that("H8: GOF rejects proportion totals and oversized expectations", {
  d <- data.frame(x = factor(rep(c("a", "b"), each = 50000)))

  expect_error(
    ifr_chisq_gof_test(d, x, c(.5, .501)),
    "expected counts in y must sum to the sample size"
  )
  expect_error(
    ifr_chisq_gof_test(data.frame(x = factor(c("a", "b"))), x,
                       c(1e308, 1e308)),
    "absolute value <= 1e150"
  )

  # a proportion total just outside one is not rescued by the sample size
  expect_error(
    ifr_chisq_gof_test(data.frame(x = factor(c("a", "a", "b", "b"))), x,
                       c(.5, .5001)),
    "expected counts in y must sum to the sample size"
  )
})

test_that("M1: GOF requires at least two categories", {
  expect_error(
    ifr_chisq_gof_test(data.frame(x = factor(rep("a", 4))), x, 4),
    "at least two categories"
  )
})

test_that("M2: Welch t test handles large finite data without overflow", {
  d <- data.frame(g = factor(c("a", "a", "b", "b")), y = (1:4) * 1e80)
  k <- ifr_ts_ind_ttest(d, g, y)

  expect_equal(k$t_satterthwaite, -2.8284)
  expect_equal(k$df_satterthwaite, 2)
  expect_equal(k$sig, 0.1056)

  d$y <- d$y / 1e80
  k2 <- ifr_ts_ind_ttest(d, g, y)
  expect_equal(k2$t_satterthwaite, k$t_satterthwaite)
  expect_equal(k2$df_satterthwaite, k$df_satterthwaite)
  expect_equal(k2$sig, k$sig)
})

test_that("L1: raw fields are documented on the remaining touched pages", {
  pages <- c(
    "ifr_chisq_assoc_test.Rd", "ifr_cochran_qtest.Rd",
    "ifr_levene_test.Rd", "ifr_ts_var_test.Rd"
  )

  for (page in pages) {
    rd <- test_path("..", "..", "man", page)
    skip_if_not(file.exists(rd))
    txt <- paste(readLines(rd), collapse = "\n")
    expect_match(txt, "_raw", fixed = TRUE)
  }

  paired <- test_path("..", "..", "man", "ifr_ts_paired_ttest.Rd")
  skip_if_not(file.exists(paired))
  ptxt <- paste(readLines(paired), collapse = "\n")
  expect_match(ptxt, "undefined", fixed = TRUE)
  expect_match(ptxt, "NA", fixed = TRUE)
})
