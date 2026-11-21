# Boundary cases restored from the 5722a08 baseline: one constant sample in the
# two-sample variance test, and an exact-fit one-way ANOVA.

test_that("RX25c: two-sample variance allows a constant first sample", {
  d <- data.frame(x = rep(2, 5), y = 1:5)

  k   <- ifr_ts_var_test(d, x, y, alternative = "all")
  ref <- stats::var.test(d$x, d$y)

  # 5722a08 values
  expect_equal(k$f, 0)
  expect_equal(k$f_raw, 0)
  expect_equal(k$lower, 0)
  expect_equal(k$upper, 1)
  expect_equal(k$two_tail, 0)
  expect_equal(unname(k$vars), c(0, 2.5))
  expect_equal(unname(k$avgs), c(2, 3))
  expect_equal(unname(k$sds), c(0, 1.58))
  expect_equal(unname(k$n1), 4)
  expect_equal(unname(k$n2), 4)
  expect_equal(k$lev, c("x", "y"))

  # base R reference
  expect_equal(k$f, round(unname(ref$statistic), 4))
  expect_equal(k$lower, round(pf(unname(ref$statistic), 4, 4), 4))
  expect_equal(k$upper,
               round(pf(unname(ref$statistic), 4, 4, lower.tail = FALSE), 4))
  expect_equal(k$two_tail, ref$p.value)

  out <- capture.output(print(k))
  expect_true(any(grepl("Pr(F < f) = 0.0000", out, fixed = TRUE)))
  expect_true(any(grepl("Pr(F > f) = 1.0000", out, fixed = TRUE)))
})

test_that("RX25c: two-sample variance allows a constant second sample", {
  d <- data.frame(x = rep(2, 5), y = 1:5)

  k   <- ifr_ts_var_test(d, y, x, alternative = "all")
  ref <- stats::var.test(d$y, d$x)

  # 5722a08 values
  expect_equal(k$f, Inf)
  expect_equal(k$f_raw, Inf)
  expect_equal(k$lower, 1)
  expect_equal(k$upper, 0)
  expect_equal(k$two_tail, 0)
  expect_equal(unname(k$vars), c(2.5, 0))
  expect_equal(unname(k$n1), 4)
  expect_equal(unname(k$n2), 4)
  expect_equal(k$lev, c("y", "x"))

  # base R reference
  expect_equal(k$f, round(unname(ref$statistic), 4))
  expect_equal(k$lower, round(pf(unname(ref$statistic), 4, 4), 4))
  expect_equal(k$upper,
               round(pf(unname(ref$statistic), 4, 4, lower.tail = FALSE), 4))
  expect_equal(k$two_tail, ref$p.value)

  out <- capture.output(print(k))
  expect_true(any(grepl("Inf", out, fixed = TRUE)))
  expect_true(any(grepl("Pr(F < f) = 1.0000", out, fixed = TRUE)))
  expect_true(any(grepl("Pr(F > f) = 0.0000", out, fixed = TRUE)))
})

test_that("RX25c: grouped two-sample variance allows one constant group", {
  d <- data.frame(v = c(rep(2, 5), 1:5),
                  g = factor(rep(c("a", "b"), each = 5)))

  first <- ifr_ts_var_test(d, v, group_var = g, alternative = "all")
  ref1  <- stats::var.test(v ~ g, data = d)

  expect_equal(first$f, 0)
  expect_equal(first$lower, 0)
  expect_equal(first$upper, 1)
  expect_equal(first$two_tail, 0)
  expect_equal(first$lev, c("a", "b"))
  expect_equal(first$f, round(unname(ref1$statistic), 4))
  expect_equal(first$two_tail, ref1$p.value)

  out1 <- capture.output(print(first))
  expect_true(any(grepl("Pr(F < f) = 0.0000", out1, fixed = TRUE)))
  expect_true(any(grepl("Pr(F > f) = 1.0000", out1, fixed = TRUE)))

  # the constant sample in the second group gives the mirror-image boundary
  d2 <- data.frame(v = c(1:5, rep(2, 5)),
                   g = factor(rep(c("a", "b"), each = 5)))
  second <- ifr_ts_var_test(d2, v, group_var = g, alternative = "all")
  ref2   <- stats::var.test(v ~ g, data = d2)

  expect_equal(second$f, Inf)
  expect_equal(second$lower, 1)
  expect_equal(second$upper, 0)
  expect_equal(second$two_tail, 0)
  expect_equal(second$f, round(unname(ref2$statistic), 4))
  expect_equal(second$two_tail, ref2$p.value)

  out2 <- capture.output(print(second))
  expect_true(any(grepl("Inf", out2, fixed = TRUE)))
  expect_true(any(grepl("Pr(F < f) = 1.0000", out2, fixed = TRUE)))
  expect_true(any(grepl("Pr(F > f) = 0.0000", out2, fixed = TRUE)))
})

test_that("RX25c: two constant groups remain undefined", {
  d <- data.frame(x = rep(2, 5), y = rep(3, 5))

  expect_error(
    ifr_ts_var_test(d, x, y),
    "both groups must have positive, finite variances"
  )

  dg <- data.frame(v = c(rep(2, 5), rep(3, 5)),
                   g = factor(rep(c("a", "b"), each = 5)))
  expect_error(
    ifr_ts_var_test(dg, v, group_var = g),
    "both groups must have positive, finite variances"
  )
})

test_that("RX25c: exact-fit one-way ANOVA returns F = Inf and p = 0", {
  d <- data.frame(x = c(1, 1, 1, 2, 2, 2),
                  g = factor(rep(c("a", "b"), each = 3)))

  # summary.lm() warns about an essentially perfect fit, as in 5722a08
  k <- suppressWarnings(ifr_oneway_anova(d, x, g))

  # 5722a08 values
  expect_equal(k$ss_between, 1.5)
  expect_equal(k$ss_within, 0)
  expect_equal(k$ss_total, 1.5)
  expect_equal(k$ms_btw, 1.5)
  expect_equal(k$ms_within, 0)
  expect_equal(k$fstat, Inf)
  expect_equal(k$pval, 0)
  expect_equal(k$pval_raw, 0)
  expect_equal(k$r2, 1)
  expect_equal(k$adjusted_r2, 1)
  expect_equal(k$rmse, 0)
  expect_equal(k$df_btw, 1)
  expect_equal(k$df_within, 4)
  expect_equal(k$df_total, 5)
  expect_equal(unname(k$group_stats$length), c(3L, 3L))
  expect_equal(unname(k$group_stats$mean), c(1, 2))
  expect_equal(unname(k$group_stats$sd), c(0, 0))

  out <- capture.output(print(k))
  expect_true(any(grepl("Inf", out, fixed = TRUE)))
  expect_true(any(grepl("Between Groups", out, fixed = TRUE)))
})

test_that("RX25c: ANOVA still errors only when total variation is zero", {
  d <- data.frame(x = rep(2, 6),
                  g = factor(rep(c("a", "b"), each = 3)))

  expect_error(
    ifr_oneway_anova(d, x, g),
    "response 'x' has no variation; a one-way ANOVA is undefined"
  )

  # a non-constant response with equal group means is a finite F = 0, not an
  # exact fit and not an error
  d2 <- data.frame(x = c(1, 2, 3, 3, 2, 1),
                   g = factor(rep(c("a", "b"), each = 3)))
  k2 <- suppressWarnings(ifr_oneway_anova(d2, x, g))
  expect_equal(k2$ss_within, 4)
  expect_equal(k2$ss_between, 0)
  expect_equal(k2$fstat, 0)
  expect_equal(k2$pval, 1)
})
