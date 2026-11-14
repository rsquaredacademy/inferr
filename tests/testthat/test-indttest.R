test_that("output from ifr_ts_ind_ttest matches expected result", {
  k <- ifr_ts_ind_ttest(hsb, female, write)
  expect_equal(k$levels, c("0", "1"))
  expect_equal(as.numeric(k$obs), c(91, 109))
  expect_equal(k$n, 200)
  expect_equal(as.numeric(k$mean), c(50.121, 54.991))
  expect_equal(as.numeric(k$sd), c(10.305, 8.134))
  expect_equal(as.numeric(k$se), c(1.080, 0.779))
  expect_equal(round(as.numeric(k$lower), 3), c(47.975, 53.447))
  expect_equal(round(as.numeric(k$upper), 3), c(52.267, 56.535))
  expect_equal(round(unlist(k$combined), 3), c(
    200.000, 52.775, 9.479, 0.670,
    199.000, 1.972, 51.453, 54.097
  ))
  expect_equal(k$mean_diff, -4.87)
  expect_equal(k$sd_dif, 9.185)
  expect_equal(k$se_dif, 1.304)
  expect_equal(round(k$conf_diff, 3), c(-7.442, -2.298))
  expect_equal(round(k$conf_diff_satterthwaite, 3), c(-7.499, -2.241))
  expect_equal(k$df_pooled, 198)
  expect_equal(k$df_satterthwaite, 170)
  expect_equal(round(k$t_pooled, 2), -3.73)
  expect_equal(round(k$t_satterthwaite, 2), -3.66)
  expect_equal(k$sig_pooled_l, 1e-04)
  expect_equal(k$sig_pooled_u, 0.9999)
  expect_equal(k$sig_pooled, 2e-04)
  expect_equal(k$sig, 3e-04)
  expect_equal(k$sig_l, 2e-04)
  expect_equal(k$sig_u, 0.9998)
  expect_equal(k$num_df, 90)
  expect_equal(k$den_df, 108)
  expect_equal(k$f, 1.6052)
  expect_equal(k$f_sig, 0.0187)
  expect_equal(k$confint, 0.95)
  expect_equal(k$var_y, "write")
  expect_equal(k$alternative, "both")
})


test_that("mean difference confidence intervals use t quantiles", {
  k <- ifr_ts_ind_ttest(hsb, female, read)

  expect_equal(
    k$conf_diff,
    round(as.numeric(t.test(read ~ female, hsb, var.equal = TRUE)$conf.int), 5)
  )
  expect_equal(
    k$conf_diff_satterthwaite,
    round(as.numeric(t.test(read ~ female, hsb, var.equal = FALSE)$conf.int), 5)
  )
})


test_that("output from independent sample t test is as expected when alternative is less", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "less"))
})

test_that("output from independent sample t test is as expected when alternative is greater", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "greater"))
})

test_that("output from independent sample t test is as expected when alternative is both", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "both"))
})

test_that("output from independent sample t test is as expected when alternative is all", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "all"))
})

test_that("ifr_ts_ind_ttest drops rows with missing values", {
  d <- hsb
  d$read[1] <- NA
  d$read[5] <- NA

  k   <- ifr_ts_ind_ttest(d, female, read)
  ref <- t.test(read ~ female, d, var.equal = TRUE)

  expect_equal(k$n, sum(complete.cases(d$female, d$read)))
  expect_equal(abs(k$t_pooled), round(abs(unname(ref$statistic)), 4))
  expect_equal(k$sig_pooled, round(ref$p.value, 4))
})

test_that("pooled SD uses n1 + n2 - 2 in the denominator (H6)", {
  d <- data.frame(
    grp = factor(rep(c("a", "b"), each = 3)),
    val = c(1, 2, 3, 4, 6, 8)
  )
  k   <- ifr_ts_ind_ttest(d, grp, val)
  ref <- sqrt(((3 - 1) * var(d$val[1:3]) + (3 - 1) * var(d$val[4:6])) /
                (3 + 3 - 2))
  tt  <- t.test(val ~ grp, d, var.equal = TRUE)

  expect_equal(k$sd_dif, round(ref, 3))
  expect_equal(k$sd_dif, 1.581)
  expect_equal(k$se_dif, round(ref * sqrt(1 / 3 + 1 / 3), 3))
  expect_equal(k$t_pooled, round(unname(tt$statistic), 4))
  expect_equal(k$conf_diff, round(as.numeric(tt$conf.int), 5))

  # nearby larger sample
  k2   <- ifr_ts_ind_ttest(hsb, female, write)
  s1   <- var(hsb$write[hsb$female == "0"])
  s2   <- var(hsb$write[hsb$female == "1"])
  ref2 <- sqrt((90 * s1 + 108 * s2) / 198)
  expect_equal(k2$sd_dif, round(ref2, 3))
})

test_that("independent t test drops unused grouping levels (M6)", {
  d <- data.frame(
    val = rep(1:3, 2),
    grp = factor(rep(c("a", "b"), each = 3), levels = c("a", "b", "unused"))
  )
  ref <- t.test(val ~ grp, d, var.equal = TRUE)
  k   <- ifr_ts_ind_ttest(d, grp, val)

  expect_equal(k$levels, c("a", "b"))
  expect_equal(k$n, 6)
  expect_equal(as.numeric(k$obs), c(3, 3))
  expect_equal(k$t_pooled, round(unname(ref$statistic), 4))
  expect_equal(k$sig_pooled, round(ref$p.value, 4))

  # an unused level must not hide a one-group design
  d2 <- data.frame(
    val = c(1, 2, 3),
    grp = factor(c("a", "a", "a"), levels = c("a", "b"))
  )
  expect_error(ifr_ts_ind_ttest(d2, grp, val),
               "x must be a binary factor variable")
})

test_that("independent t test works when columns are named x or y (H8)", {
  d <- data.frame(
    y   = c(1, 2, 3, 5, 6, 7),
    grp = factor(c("a", "a", "a", "b", "b", "b"))
  )
  ref <- t.test(y ~ grp, d, var.equal = TRUE)

  k  <- ifr_ts_ind_ttest(d, grp, y)
  expect_equal(k$t_pooled, round(unname(ref$statistic), 4))
  expect_equal(k$sig_pooled, round(ref$p.value, 4))

  d2 <- setNames(d, c("x", "grp"))
  k2 <- ifr_ts_ind_ttest(d2, grp, x)
  expect_equal(k2$t_pooled, round(unname(ref$statistic), 4))
  expect_equal(k2$sig_pooled, round(ref$p.value, 4))
})
