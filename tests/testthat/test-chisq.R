test_that("ouput from ifr_chisq_assoc_test matches the expected result", {
  k <- ifr_chisq_assoc_test(hsb, female, schtyp)
  expect_equal(k$df, 1)
  expect_equal(k$chisquare, 0.047)
  expect_equal(k$chisquare_lr, 0.0471)
  expect_equal(k$chisquare_mantel_haenszel, 0.0468)
  expect_equal(k$chisquare_adjusted, 0.0005)
  expect_equal(k$pval_chisquare, 0.8283)
  expect_equal(k$pval_chisquare_lr, 0.8281)
  expect_equal(k$pval_chisquare_mantel_haenszel, 0.8287)
  expect_equal(k$pval_chisquare_adjusted, 0.9815)
  expect_equal(k$phi_coefficient, 0.0153)
  expect_equal(k$contingency_coefficient, 0.0153)
  expect_equal(k$cramers_v, 0.0153)
  expect_equal(k$ds, 4)
})

test_that("association p-values use the unrounded chi-square (L4)", {
  m    <- matrix(c(1, 1, 2, 3), 2)
  long <- as.data.frame(as.table(m))
  d    <- data.frame(x = factor(rep(long$Var1, long$Freq)),
                     y = factor(rep(long$Var2, long$Freq)))

  k   <- ifr_chisq_assoc_test(d, x, y)
  ref <- suppressWarnings(chisq.test(m, correct = FALSE))

  expect_equal(k$chisquare, round(unname(ref$statistic), 4))
  expect_equal(k$pval_chisquare, round(ref$p.value, 4))
  expect_equal(k$pval_chisquare, 0.8091)

  # likelihood ratio and Mantel-Haenszel p-values from the raw statistics
  e  <- outer(rowSums(m), colSums(m)) / sum(m)
  lr <- 2 * sum(ifelse(m == 0, 0, m * log(m / e)))
  expect_equal(k$pval_chisquare_lr,
               round(pchisq(lr, k$df, lower.tail = FALSE), 4))

  num <- m[1, 1] - (rowSums(m)[1] * colSums(m)[1] / sum(m))
  den <- (prod(rowSums(m)) * prod(colSums(m))) /
    (sum(m) ^ 3 - sum(m) ^ 2)
  mh  <- num ^ 2 / den
  expect_equal(k$pval_chisquare_mantel_haenszel,
               round(pchisq(mh, 1, lower.tail = FALSE), 4))
})

test_that("ouput from ifr_chisq_assoc_test matches the expected result", {
  k <- ifr_chisq_assoc_test(hsb, female, ses)
  expect_equal(k$df, 2)
  expect_equal(k$chisquare, 4.5765)
  expect_equal(k$chisquare_lr, 4.6789)
  expect_equal(k$pval_chisquare, 0.1014)
  expect_equal(k$pval_chisquare_lr, 0.0964)
  expect_true(is.na(k$phi_coefficient))
  expect_equal(k$contingency_coefficient, 0.1496)
  expect_equal(k$cramers_v, 0.1513)
  expect_equal(k$ds, 6)
})

test_that("ifr_chisq_assoc_test throws the appropriate error", {
  expect_error(ifr_chisq_assoc_test(hsb, female, read), "y must be a categorical variable")
  expect_error(ifr_chisq_assoc_test(hsb, read, ses), "x must be a categorical variable")
})

test_that("likelihood ratio chi square handles empty cells", {
  d <- data.frame(
    x = factor(c(rep("a", 20), rep("b", 20)), levels = c("a", "b")),
    y = factor(c(rep("c", 20), rep("d", 20)), levels = c("c", "d"))
  )
  k <- ifr_chisq_assoc_test(d, x, y)
  t <- table(d$x, d$y)
  e <- outer(rowSums(t), colSums(t)) / sum(t)
  lr <- 2 * sum(ifelse(t == 0, 0, t * log(t / e)))

  expect_equal(k$chisquare_lr, round(lr, 4))
  expect_equal(k$chisquare_lr, 55.4518)
})

test_that("phi coefficient is NA for a non-2x2 table", {
  k <- ifr_chisq_assoc_test(hsb, female, ses)

  expect_true(is.na(k$phi_coefficient))

  # independent computation of Cramer's V
  tbl <- table(hsb$female, hsb$ses)
  chi <- unname(chisq.test(tbl, correct = FALSE)$statistic)
  cv  <- sqrt(chi / sum(tbl) / (min(dim(tbl)) - 1))

  expect_equal(k$cramers_v, round(cv, 4))
})

test_that("ouput from ifr_chisq_assoc_test is as expected", {
  expect_snapshot(ifr_chisq_assoc_test(hsb, female, schtyp))
})

test_that("ouput from ifr_chisq_assoc_test 2 is as expected", {
  expect_snapshot(ifr_chisq_assoc_test(hsb, female, ses))
})

test_that("Yates correction is bounded by the observed deviation (H3)", {
  d <- data.frame(
    x = factor(rep(c("a", "b"), each = 10)),
    y = factor(rep(c("a", "b"), 10))
  )
  k   <- ifr_chisq_assoc_test(d, x, y)
  ref <- chisq.test(table(d$x, d$y))

  expect_equal(k$chisquare_adjusted, unname(ref$statistic))
  expect_equal(k$pval_chisquare_adjusted, ref$p.value)
  expect_equal(k$chisquare_adjusted, 0)
  expect_equal(k$pval_chisquare_adjusted, 1)

  # a non-zero table still follows stats::chisq.test()
  k2  <- ifr_chisq_assoc_test(hsb, female, schtyp)
  ref2 <- chisq.test(table(hsb$female, hsb$schtyp))
  expect_equal(k2$chisquare_adjusted, round(unname(ref2$statistic), 4))
  expect_equal(k2$pval_chisquare_adjusted, round(ref2$p.value, 4))
})
