# Ordinary teaching-input regression grid. Every touched interface is compared
# against its base R reference on mtcars/hsb-style data. This file must stay
# green; it guards against regressions from the rx25 fixes.

test_that("ordinary ANOVA matches anova(lm())", {
  k   <- ifr_oneway_anova(mtcars, mpg, cyl)
  ref <- anova(lm(mpg ~ as.factor(cyl), data = mtcars))

  expect_equal(k$ss_between, round(ref[["Sum Sq"]][1], 3))
  expect_equal(k$ss_within, round(ref[["Sum Sq"]][2], 3))
  expect_equal(k$ss_total, round(sum(ref[["Sum Sq"]]), 3))
  expect_equal(k$df_btw, ref$Df[1])
  expect_equal(k$df_within, ref$Df[2])
  expect_equal(k$fstat, round(ref[["F value"]][1], 3))
  expect_equal(k$pval, ref[["Pr(>F)"]][1], tolerance = 1e-10)
  expect_equal(k$r2, round(summary(lm(mpg ~ as.factor(cyl),
                                      data = mtcars))$r.squared, 4))
})

test_that("ordinary goodness-of-fit matches chisq.test()", {
  obs <- as.vector(table(hsb$race))
  k   <- ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
  ref <- chisq.test(obs, p = c(20, 20, 20, 140) / 200)

  expect_equal(k$observed_frequency, obs)
  expect_equal(k$expected_frequency, c(20, 20, 20, 140))
  expect_equal(k$chisquare, round(unname(ref$statistic), 4))
  expect_equal(k$pvalue, round(ref$p.value, 4))
  expect_equal(k$degrees_of_freedom, unname(ref$parameter))
  expect_equal(k$sample_size, 200)
})

test_that("ordinary McNemar matches mcnemar.test()", {
  m   <- matrix(c(135, 18, 21, 26), 2)
  k   <- ifr_mcnemar_test(m)
  ref <- mcnemar.test(m, correct = FALSE)
  b   <- m[1, 2]
  cc  <- m[2, 1]
  ex  <- min(1, 2 * min(pbinom(b, b + cc, .5), pbinom(cc, b + cc, .5)))

  expect_equal(k$statistic, round(unname(ref$statistic), 4))
  expect_equal(k$pvalue, round(ref$p.value, 4))
  expect_equal(k$exactp, round(ex, 4))
  expect_equal(k$df, unname(ref$parameter))
})

test_that("ordinary one-sample t matches t.test()", {
  k   <- ifr_os_t_test(hsb, write, mu = 50)
  ref <- t.test(hsb$write, mu = 50)

  expect_equal(k$n, length(hsb$write))
  expect_equal(k$Mean, round(mean(hsb$write), 4))
  expect_equal(k$stddev, round(sd(hsb$write), 4))
  expect_equal(k$test_stat, round(unname(ref$statistic), 3))
  expect_equal(k$p, ref$p.value, tolerance = 1e-10)
  expect_equal(k$confint, round(as.numeric(ref$conf.int), 4))
})

test_that("ordinary one-sample variance matches the chi-square formula", {
  x   <- mtcars$mpg
  n   <- length(x)
  k   <- ifr_os_var_test(mtcars, mpg, 5)
  chi <- (n - 1) * var(x) / 25
  p_l <- pchisq(chi, n - 1)
  p_u <- pchisq(chi, n - 1, lower.tail = FALSE)
  p_t <- 2 * min(p_l, p_u)
  a   <- .025
  tv  <- (n - 1) * var(x)

  expect_equal(k$chi, round(chi, 4))
  expect_equal(k$p_lower, p_l, tolerance = 1e-12)
  expect_equal(k$p_upper, p_u, tolerance = 1e-12)
  expect_equal(k$p_two, p_t, tolerance = 1e-12)
  expect_equal(k$c_lwr, round(sqrt(tv / qchisq(1 - a, n - 1)), 4))
  expect_equal(k$c_upr, round(sqrt(tv / qchisq(a, n - 1)), 4))
})

test_that("ordinary independent t matches t.test()", {
  k     <- ifr_ts_ind_ttest(hsb, female, write)
  pooled <- t.test(write ~ female, hsb, var.equal = TRUE)
  welch  <- t.test(write ~ female, hsb)

  expect_equal(k$t_pooled, round(unname(pooled$statistic), 4))
  expect_equal(k$t_satterthwaite, round(unname(welch$statistic), 4))
  expect_equal(k$df_satterthwaite, unname(welch$parameter), tolerance = 1e-10)
  expect_equal(k$sig_pooled, round(pooled$p.value, 4))
  expect_equal(k$sig, round(welch$p.value, 4))
  expect_equal(k$conf_diff, round(as.numeric(pooled$conf.int), 5))
  expect_equal(k$conf_diff_satterthwaite, round(as.numeric(welch$conf.int), 5))
})

test_that("ordinary paired t matches t.test(paired = TRUE)", {
  k   <- ifr_ts_paired_ttest(hsb, read, write)
  ref <- t.test(hsb$read, hsb$write, paired = TRUE)

  expect_equal(k$Obs, sum(complete.cases(hsb$read, hsb$write)))
  expect_equal(k$tstat, round(unname(ref$statistic), 4))
  expect_equal(k$p_two_tail, ref$p.value, tolerance = 1e-10)
  expect_equal(k$corr, round(cor(hsb$read, hsb$write), 2))
})

test_that("ordinary two-sample proportions match prop.test()", {
  mtcarz <- mtcars
  mtcarz$vs <- as.factor(mtcarz$vs)
  mtcarz$am <- as.factor(mtcarz$am)

  k <- ifr_ts_prop_test(mtcarz, am, vs)
  n1 <- sum(!is.na(mtcarz$am))
  n2 <- sum(!is.na(mtcarz$vs))
  y1 <- sum(mtcarz$am == "1", na.rm = TRUE)
  y2 <- sum(mtcarz$vs == "1", na.rm = TRUE)
  ref <- prop.test(c(y1, y2), c(n1, n2), correct = FALSE)

  expect_equal(c(k$n1, k$n2), c(n1, n2))
  expect_equal(c(k$phat1, k$phat2), round(c(y1 / n1, y2 / n2), 4))
  expect_equal(abs(k$z), round(sqrt(unname(ref$statistic)), 3))
  expect_equal(k$sig, round(ref$p.value, 3))
})

test_that("ordinary grouped proportions match prop.test()", {
  mtcarz <- mtcars
  mtcarz$vs <- as.factor(mtcarz$vs)
  mtcarz$am <- as.factor(mtcarz$am)

  k <- ifr_ts_prop_group(mtcarz, am, vs)
  tab <- table(mtcarz$vs, mtcarz$am)
  ref <- prop.test(tab[, "1"], rowSums(tab), correct = FALSE)

  expect_equal(c(k$n1, k$n2), unname(rowSums(tab)))
  expect_equal(c(k$phat1, k$phat2), unname(tab[, "1"] / rowSums(tab)))
  expect_equal(abs(k$z), round(sqrt(unname(ref$statistic)), 3))
  expect_equal(k$sig, round(ref$p.value, 3))
})

test_that("ordinary proportion calculator matches the pooled z formula", {
  k    <- ifr_ts_prop_calc(30, 25, .3, .5)
  phat <- (30 * .3 + 25 * .5) / 55
  z    <- (.3 - .5) / sqrt(phat * (1 - phat) * (1 / 30 + 1 / 25))

  expect_equal(k$z, round(z, 3))
  expect_equal(k$sig, round(2 * pnorm(abs(z), lower.tail = FALSE), 3))
})

test_that("ordinary one-sample proportion matches prop.test()", {
  k   <- ifr_os_prop_test(hsb, female, prob = .5)
  ref <- prop.test(sum(hsb$female == "1"), length(hsb$female),
                   p = .5, correct = FALSE)
  z   <- (mean(hsb$female == "1") - .5) / sqrt(.25 / length(hsb$female))

  expect_equal(k$n, length(hsb$female))
  expect_equal(k$phat, round(mean(hsb$female == "1"), 4))
  expect_equal(k$z, round(z, 4))
  expect_equal(k$sig, round(ref$p.value, 4))
})
