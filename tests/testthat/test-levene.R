test_that("output from ifr_levene_test matches the expected result", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)

  k <- ifr_levene_test(mt, mpg, group_var = cyl)
  expect_equal(k$bf, 5.5071)
  expect_equal(k$p_bf, 0.0094)
  expect_equal(k$lev, 6.4843)
  expect_equal(k$p_lev, 0.0047)
  expect_equal(k$bft, 6.2047)
  expect_equal(k$p_bft, 0.0057)
  expect_equal(as.vector(k$avgs), c(26.66, 19.74, 15.10))
  expect_equal(as.vector(k$sds), c(4.51, 1.45, 2.56))
  expect_equal(k$avg, 20.09)
  expect_equal(k$sd, 6.03)
  expect_equal(k$n, 32)
  expect_equal(k$levs, c("4", "6", "8"))
  expect_equal(k$n_df, 2)
  expect_equal(k$d_df, 29)
  expect_equal(as.vector(k$lens), c(11, 7, 14))
})



test_that("output from ifr_levene_test matches the expected result", {
  k <- ifr_levene_test(mtcars, mpg, qsec)
  expect_equal(k$bf, 20.9464)
  expect_equal(k$p_bf, 0)
  expect_equal(k$lev, 24.3932)
  expect_equal(k$p_lev, 0)
  expect_equal(k$bft, 22.7064)
  expect_equal(k$p_bft, 0)
  expect_equal(as.vector(k$avgs), c(20.09, 17.85))
  expect_equal(as.vector(k$sds), c(6.03, 1.79))
  expect_equal(k$avg, 18.97)
  expect_equal(k$sd, 4.55)
  expect_equal(k$n, 64)
  expect_equal(k$levs, c("0", "1"))
  expect_equal(k$n_df, 1)
  expect_equal(k$d_df, 62)
  expect_equal(as.vector(k$lens), c(32, 32))
})


test_that("output from levene test is as expected", {
  expect_snapshot(ifr_levene_test(hsb, read, group_var = race))
})

test_that("ifr_levene_test works with a character grouping variable", {
  mt <- mtcars
  mt$cyl_chr <- as.character(mt$cyl)

  k <- ifr_levene_test(mt, mpg, group_var = cyl_chr)

  expect_equal(k$n_df, 2)
  expect_equal(k$d_df, 29)
  expect_equal(k$levs, c("4", "6", "8"))

  # independent computation: one-way ANOVA on |x - group mean|
  g   <- ave(mt$mpg, mt$cyl_chr, FUN = mean)
  ref <- anova(lm(abs(mt$mpg - g) ~ mt$cyl_chr))

  expect_equal(k$lev, round(ref[["F value"]][1], 4))
  expect_equal(k$p_lev, round(ref[["Pr(>F)"]][1], 4))
})

test_that("ifr_levene_test centres Levene on the mean and Brown-Forsythe on the median (H5)", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)
  k <- ifr_levene_test(mt, mpg, group_var = cyl)

  zmean  <- abs(mt$mpg - ave(mt$mpg, mt$cyl, FUN = mean))
  zmedian <- abs(mt$mpg - ave(mt$mpg, mt$cyl, FUN = median))
  mean_ref   <- anova(lm(zmean ~ mt$cyl))
  median_ref <- anova(lm(zmedian ~ mt$cyl))

  expect_equal(k$lev, round(mean_ref[["F value"]][1], 4))
  expect_equal(k$p_lev, round(mean_ref[["Pr(>F)"]][1], 4))
  expect_equal(k$bf, round(median_ref[["F value"]][1], 4))
  expect_equal(k$p_bf, round(median_ref[["Pr(>F)"]][1], 4))
  expect_false(isTRUE(all.equal(k$bf, k$lev)))

  # nearby case with two groups and missing values
  mt2 <- mtcars
  mt2$am <- as.factor(mt2$am)
  mt2$mpg[c(2, 20)] <- NA
  k2 <- ifr_levene_test(mt2, mpg, group_var = am)
  ok <- complete.cases(mt2$mpg, mt2$am)
  mean_ref2   <- anova(lm(abs(mt2$mpg[ok] - ave(mt2$mpg[ok], mt2$am[ok], FUN = mean)) ~ mt2$am[ok]))
  median_ref2 <- anova(lm(abs(mt2$mpg[ok] - ave(mt2$mpg[ok], mt2$am[ok], FUN = median)) ~ mt2$am[ok]))
  expect_equal(k2$lev, round(mean_ref2[["F value"]][1], 4))
  expect_equal(k2$bf, round(median_ref2[["F value"]][1], 4))
})

test_that("ifr_levene_test counts complete observations when data contain NA", {
  mt <- mtcars
  mt$cyl <- as.factor(mt$cyl)
  mt$mpg[1:5] <- NA

  k <- ifr_levene_test(mt, mpg, group_var = cyl)

  expect_equal(k$n, 27)
  expect_equal(k$d_df, 24)
  expect_equal(k$n_df, 2)

  # independent computation on the complete cases
  ok  <- complete.cases(mt$mpg, mt$cyl)
  g   <- ave(mt$mpg[ok], mt$cyl[ok], FUN = mean)
  ref <- anova(lm(abs(mt$mpg[ok] - g) ~ mt$cyl[ok]))

  expect_equal(k$lev, round(ref[["F value"]][1], 4))
})
