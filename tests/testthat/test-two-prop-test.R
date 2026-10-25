mtcarz <- mtcars
mtcarz$vs <- as.factor(mtcarz$vs)
mtcarz$am <- as.factor(mtcarz$am)

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_test(mtcarz, am, vs, alternative = "less")
  expect_equal(k$n1, 32)
  expect_equal(k$n2, 32)
  expect_equal(k$phat1, 0.4062)
  expect_equal(k$phat2, 0.4375)
  expect_equal(round(k$z, 3), -0.254)
  expect_equal(round(k$sig, 3), 0.4)
  expect_equal(k$alt, "less")
})


test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_test(
    mtcarz, am, vs,
    alternative = "greater"
  )
  expect_equal(round(k$sig, 3), 0.6)
  expect_equal(k$alt, "greater")
})


test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_test(
    mtcarz, am, vs,
    alternative = "both"
  )
  expect_equal(round(k$sig, 3), 0.8)
  expect_equal(k$alt, "both")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_test(
    mtcarz, am, vs,
    alternative = "all"
  )
  expect_equal(unname(round(k$sig, 3)), c(0.8, 0.4, 0.6))
  expect_equal(k$alt, "all")
})


test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_group(mtcarz, am, vs, alternative = "less")
  expect_equal(k$n1, 18)
  expect_equal(k$n2, 14)
  expect_equal(round(k$phat1, 3), 0.333)
  expect_equal(k$phat2, 0.5)
  expect_equal(round(k$z, 3), -0.952)
  expect_equal(round(k$sig, 3), 0.17)
  expect_equal(k$alt, "less")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_group(
    mtcarz, am, vs,
    alternative = "greater"
  )
  expect_equal(round(k$sig, 3), 0.83)
  expect_equal(k$alt, "greater")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_group(
    mtcarz, am, vs,
    alternative = "both"
  )
  expect_equal(round(k$sig, 3), 0.341)
  expect_equal(k$alt, "both")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_group(
    mtcarz, am, vs,
    alternative = "all"
  )
  expect_equal(unname(round(k$sig, 3)), c(0.341, 0.17, 0.83))
  expect_equal(k$alt, "all")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_calc(
    n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
    alternative = "less"
  )
  expect_equal(k$n1, 30)
  expect_equal(k$n2, 25)
  expect_equal(round(k$phat1, 3), 0.3)
  expect_equal(k$phat2, 0.5)
  expect_equal(round(k$z, 3), -1.514)
  expect_equal(round(k$sig, 3), 0.065)
  expect_equal(k$alt, "less")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_calc(
    n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
    alternative = "greater"
  )
  expect_equal(round(k$sig, 3), 0.935)
  expect_equal(k$alt, "greater")
})

test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_calc(
    n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
    alternative = "both"
  )
  expect_equal(round(k$sig, 3), 0.13)
  expect_equal(k$alt, "both")
})


test_that("output from ifr_ts_prop_test matches the expected result", {
  k <- ifr_ts_prop_calc(
    n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
    alternative = "all"
  )
  expect_equal(unname(round(k$sig, 3)), c(0.13, 0.065, 0.935))
  expect_equal(k$alt, "all")
})

test_that("output from 2 sample proportion test is as expected when alternative is less", {
  expect_snapshot(ifr_ts_prop_group(treatment2, outcome, female, alternative = "less"))
})

test_that("output from 2 sample proportion test is as expected when alternative is greater", {
  expect_snapshot(ifr_ts_prop_group(treatment2, outcome, female, alternative = "greater"))
})

test_that("output from 2 sample proportion test is as expected when alternative is both", {
  expect_snapshot(ifr_ts_prop_group(treatment2, outcome, female, alternative = "both"))
})

test_that("output from 2 sample proportion test is as expected when alternative is all", {
  expect_snapshot(ifr_ts_prop_group(treatment2, outcome, female, alternative = "all"))
})

test_that("ifr_ts_prop_test requires binary variables", {
  d <- data.frame(
    a = factor(rep(c("a", "b", "c"), 10)),
    b = factor(rep(c("x", "y"), 15))
  )

  expect_error(ifr_ts_prop_test(d, a, b), "var1 must be a binary variable")
  expect_error(ifr_ts_prop_test(d, b, a), "var2 must be a binary variable")

  d2 <- data.frame(
    a = factor(c(rep("yes", 90), rep("no", 110))),
    b = factor(c(rep("yes", 100), rep("no", 100)))
  )

  k   <- ifr_ts_prop_test(d2, a, b)
  ref <- prop.test(c(90, 100), c(200, 200), correct = FALSE)

  expect_equal(k$phat1, 0.45)
  expect_equal(k$phat2, 0.5)
  expect_equal(abs(k$z), round(sqrt(unname(ref$statistic)), 3))
})

test_that("ifr_ts_prop_group requires a binary variable and group", {
  d <- data.frame(
    out = factor(rep(c("a", "b", "c"), 20)),
    grp = rep(c("a", "b"), 30)
  )
  expect_error(ifr_ts_prop_group(d, out, grp), "var must be a binary variable")

  d2 <- data.frame(
    out = factor(rep(c("a", "b"), 30)),
    grp = rep(c("a", "b", "c"), 20)
  )
  expect_error(ifr_ts_prop_group(d2, out, grp), "group must be a binary variable")

  d3 <- data.frame(
    out = factor(c(rep("yes", 40), rep("no", 60), rep("yes", 30), rep("no", 70))),
    grp = c(rep("a", 100), rep("b", 100))
  )

  k   <- ifr_ts_prop_group(d3, out, grp)
  ref <- prop.test(c(40, 30), c(100, 100), correct = FALSE)

  expect_equal(k$n1, 100)
  expect_equal(k$n2, 100)
  expect_equal(k$phat1, 0.4)
  expect_equal(k$phat2, 0.3)
  expect_equal(k$z, round(sqrt(unname(ref$statistic)), 3))
})

test_that("two sample proportion output labels the two-sided p-value correctly", {
  out <- capture.output(
    ifr_ts_prop_group(treatment2, outcome, female, alternative = "both")
  )

  expect_true(any(grepl("Pr\\(\\|Z\\| > \\|z\\|\\)", out)))
  expect_false(any(grepl("Pr\\(\\|Z\\| < \\|z\\|\\)", out)))
})
