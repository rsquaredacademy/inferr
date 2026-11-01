test_that("output from ifr_oneway_anova matches the expected output", {
  k <- ifr_oneway_anova(mtcars, mpg, cyl)
  expect_equal(k$ss_between, 824.785)
  expect_equal(k$ss_within, 301.263)
  expect_equal(k$ss_total, 1126.047)
  expect_equal(k$df_btw, 2)
  expect_equal(k$df_within, 29)
  expect_equal(k$df_total, 31)
  expect_equal(k$ms_btw, 412.392)
  expect_equal(k$ms_within, 10.388)
  expect_equal(k$fstat, 39.698)
  expect_equal(k$pval, 4.97891916584e-09)
  expect_equal(k$r2, 0.7325)
  expect_equal(k$adjusted_r2, 0.714)
  expect_equal(k$rmse, 3.2231)
  expect_equal(k$obs, 32)
})

test_that("ifr_oneway_anova drops incomplete cases", {
  d      <- mtcars
  d$mpg[1] <- NA
  k      <- ifr_oneway_anova(d, mpg, cyl)
  base   <- anova(lm(mpg ~ as.factor(cyl), data = d))

  expect_equal(k$obs, 31)
  expect_equal(k$df_within, base$Df[2])
  expect_equal(k$df_total, sum(base$Df))
  expect_equal(k$fstat, round(base$`F value`[1], 3))
  expect_equal(k$pval, base$`Pr(>F)`[1])
})

test_that("output from one way anova is as expected", {
  expect_snapshot(ifr_oneway_anova(mtcars, mpg, cyl))
})

test_that("ifr_oneway_anova keeps the Sig. column inside the table", {
  for (x in list(ifr_oneway_anova(mtcars, mpg, cyl),
                 ifr_oneway_anova(hsb, write, prog))) {
    out  <- capture.output(print(x))
    row  <- grep("^Between Groups", out, value = TRUE)
    rule <- grep("^-+$", out, value = TRUE)

    expect_equal(nchar(row), nchar(rule[[1]]))
    expect_match(row, "e-09 *$")
  }
})

test_that("a singleton group does not make the ANOVA missing (H7)", {
  d <- data.frame(
    val = c(1, 2, 4, 10),
    grp = factor(c("a", "a", "a", "b"))
  )
  k   <- ifr_oneway_anova(d, val, grp)
  ref <- anova(lm(val ~ grp, d))

  expect_equal(k$ss_within, round(ref[["Sum Sq"]][2], 3))
  expect_equal(k$ss_within, 4.667)
  expect_equal(k$df_within, ref$Df[2])
  expect_equal(k$fstat, round(ref[["F value"]][1], 3))
  expect_equal(k$fstat, 18.893)
  expect_equal(k$pval, ref[["Pr(>F)"]][1], tolerance = 1e-10)

  # nearby case: two singleton groups and one larger group
  d2 <- data.frame(
    val = c(1, 2, 4, 5, 10),
    grp = factor(c("a", "a", "a", "b", "c"))
  )
  k2   <- ifr_oneway_anova(d2, val, grp)
  ref2 <- anova(lm(val ~ grp, d2))
  expect_equal(k2$ss_within, round(ref2[["Sum Sq"]][2], 3))
  expect_equal(k2$pval, ref2[["Pr(>F)"]][1], tolerance = 1e-10)
})

test_that("ANOVA rejects a design with no residual degrees of freedom (H7)", {
  d <- data.frame(
    val = 1:3,
    grp = factor(c("a", "b", "c"))
  )
  expect_error(ifr_oneway_anova(d, val, grp), "residual degrees of freedom")
})

test_that("ANOVA works when columns are named x or y (H8)", {
  d <- data.frame(
    v   = c(1, 2, 3, 5, 6, 7),
    grp = factor(c("a", "a", "a", "b", "b", "b"))
  )
  ref <- anova(lm(v ~ grp, d))

  dx <- setNames(d, c("x", "grp"))
  kx <- ifr_oneway_anova(dx, x, grp)
  expect_equal(kx$pval, ref[["Pr(>F)"]][1], tolerance = 1e-10)

  dy <- setNames(d, c("y", "grp"))
  ky <- ifr_oneway_anova(dy, y, grp)
  expect_equal(ky$pval, ref[["Pr(>F)"]][1], tolerance = 1e-10)
})

test_that("ANOVA p-value uses the upper tail without cancellation (H10)", {
  d <- data.frame(
    val = c(rep(0, 5), rep(100, 5)) + rep(c(-.2, -.1, 0, .1, .2), 2),
    grp = rep(c("a", "b"), each = 5)
  )
  k   <- ifr_oneway_anova(d, val, grp)
  ref <- anova(lm(val ~ grp, d))

  expect_gt(k$pval, 0)
  expect_equal(k$pval, ref[["Pr(>F)"]][1], tolerance = 1e-12)
  expect_equal(
    k$pval,
    pf(ref[["F value"]][1], ref$Df[1], ref$Df[2], lower.tail = FALSE),
    tolerance = 1e-12
  )
})
