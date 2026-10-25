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
