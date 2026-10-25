test_that("output from ifr_mcnemar_test matches the expected result", {
  k <- ifr_mcnemar_test(matrix(c(172, 7, 6, 15), nrow = 2))
  expect_equal(k$statistic, 0.0769)
  expect_equal(k$df, 1.000)
  expect_equal(k$pvalue, 0.7815)
  expect_equal(k$exactp, 1.0000)
  expect_equal(k$kappa, 0.6613)
  expect_equal(k$std_err, 0.0873)
  expect_equal(k$kappa_cil, 0.4901)
  expect_equal(k$kappa_ciu, 0.8324)
  expect_equal(k$cases, 0.8900)
  expect_equal(k$controls, 0.8950)
  expect_equal(k$ratio, 0.9944)
  expect_equal(k$odratio, 0.8571)
})


test_that("output from ifr_mcnemar_test matches the expected result", {
  k <- ifr_mcnemar_test(table(hsb$female, hsb$schtyp))
  expect_equal(k$statistic, 56.4667)
  expect_equal(k$df, 1)
  expect_equal(k$pvalue, 0)
  expect_equal(k$exactp, 0)
  expect_equal(k$kappa, 0.0106)
  expect_equal(unname(k$std_err), 0.0485)
  expect_equal(unname(k$kappa_cil), -0.0846)
  expect_equal(unname(k$kappa_ciu), 0.1057)
  expect_equal(unname(k$cases), 0.455)
  expect_equal(unname(k$controls), 0.84)
  expect_equal(unname(k$ratio), 0.5417)
  expect_equal(k$odratio, 0.1538)
})


test_that("output from mcnemar test is as expected", {
  himath <- ifelse(hsb$math > 60, 1, 0)
  hiread <- ifelse(hsb$read > 60, 1, 0)
  expect_snapshot(ifr_mcnemar_test(table(himath, hiread)))
})

test_that("ifr_mcnemar_test requires a 2 x 2 table and matches mcnemar.test", {
  d <- data.frame(
    a = factor(c(rep("0", 20), rep("1", 10), rep("0", 5), rep("1", 15))),
    b = factor(c(rep("0", 20), rep("0", 10), rep("1", 5), rep("1", 15)))
  )

  k    <- ifr_mcnemar_test(d, a, b)
  tbl  <- table(d$a, d$b)
  ref1 <- mcnemar.test(tbl, correct = FALSE)
  ref2 <- mcnemar.test(tbl, correct = TRUE)

  expect_equal(k$statistic, round(unname(ref1$statistic), 4))
  expect_equal(k$pvalue, round(ref1$p.value, 4))
  expect_equal(k$cstat, unname(ref2$statistic), tolerance = 1e-10)
  expect_equal(k$cpvalue, ref2$p.value, tolerance = 1e-10)

  d3 <- data.frame(
    a = factor(rep(c("a", "b", "c"), 4)),
    b = factor(rep(c("a", "b", "c"), each = 4))
  )
  expect_error(ifr_mcnemar_test(d3, a, b), "exactly two levels")
})

test_that("McNemar output aligns the statistic and degrees of freedom", {
  out <- capture.output(ifr_mcnemar_test(matrix(c(0, 1, 100, 0), nrow = 2)))

  stat_line <- out[grep("McNemar's chi2", out)]
  df_line   <- out[grep("^DF", out)]

  expect_equal(nchar(df_line), nchar(stat_line))
})
