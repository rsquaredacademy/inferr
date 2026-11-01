test_that("output from ifr_os_prop_test matches expected result", {
  k <- ifr_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = "both")
  expect_equal(k$n, 200)
  expect_equal(k$phat, 0.3)
  expect_equal(k$p, 0.5)
  expect_equal(k$z, -5.6569)
  expect_equal(k$sig, 0)
  expect_equal(k$alt, "both")
  expect_equal(k$obs, c(140, 60))
  expect_equal(k$exp, c(100, 100))
  expect_equal(k$deviation, c(" 40.00", "-40.00"))
  expect_equal(k$std, c(" 4.00", "-4.00"))
})

test_that("output from ifr_os_prop_test matches expected result when using factor variables", {
  k <- ifr_os_prop_test(hsb, female, prob = 0.5)
  expect_equal(k$n, 200)
  expect_equal(k$phat, 0.545)
  expect_equal(k$p, 0.5)
  expect_equal(k$z, 1.2728)
  expect_equal(k$sig, 0.2031)
  expect_equal(k$alt, "both")
  expect_equal(k$obs, c(91, 109))
  expect_equal(k$exp, c(100, 100))
  expect_equal(k$deviation, c("-9.00", " 9.00"))
  expect_equal(k$std, c("-0.90", " 0.90"))
})

test_that("output from ifr_os_prop_test matches expected result", {
  k <- ifr_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = "less")
  expect_equal(k$sig, 0)
  expect_equal(k$alt, "less")
})

test_that("output from ifr_os_prop_test matches expected result", {
  k <- ifr_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = "greater")
  expect_equal(k$sig, 1)
  expect_equal(k$alt, "greater")
})


test_that("output from ifr_os_prop_test matches expected result", {
  k <- ifr_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = "all")
  expect_equal(unname(k$sig), c(0, 0, 1))
  expect_equal(k$alt, "all")
})

test_that("ifr_os_prop_test requires a binary variable and matches prop.test", {
  d <- data.frame(out = factor(rep(c("a", "b", "c"), 20)))
  expect_error(ifr_os_prop_test(d, out), "variable must be a binary variable")

  d2 <- data.frame(out = factor(c(rep("yes", 120), rep("no", 80))))
  k  <- ifr_os_prop_test(d2, out)
  ref <- prop.test(120, 200, p = 0.5, correct = FALSE)

  expect_equal(k$phat, 0.6)
  expect_equal(k$z, round(sqrt(unname(ref$statistic)), 4))
  expect_equal(k$sig, round(ref$p.value, 4))
})

test_that("ifr_os_prop_test rejects invalid proportions and sample sizes (M5)", {
  expect_error(ifr_os_prop_test(100, phat = 2), "phat must be between 0 and 1")
  expect_error(ifr_os_prop_test(100, phat = -0.1), "phat must be between 0 and 1")
  expect_error(ifr_os_prop_test(100, phat = 0.5, prob = 1.5),
               "prob must be between 0 and 1")
  expect_error(ifr_os_prop_test(0, phat = 0.5), "n must be a positive integer")
  expect_error(ifr_os_prop_test(-5, phat = 0.5), "n must be a positive integer")

  d <- data.frame(out = factor(c(NA, NA), levels = c("no", "yes")))
  expect_error(ifr_os_prop_test(d, out), "at least one non-missing")

  # boundaries remain valid
  k <- ifr_os_prop_test(100, phat = 0)
  expect_equal(k$obs, c(100, 0))
  expect_equal(unname(k$sig), 0)
})

test_that("ifr_os_prop_test ignores missing outcomes (H1)", {
  d <- data.frame(x = factor(c("no", "yes", "yes", NA),
                             levels = c("no", "yes")))
  k   <- ifr_os_prop_test(d, x)
  ref <- suppressWarnings(prop.test(2, 3, correct = FALSE))

  expect_equal(k$n, 3)
  expect_equal(k$phat, round(2 / 3, 4))
  expect_equal(k$obs, c(1, 2))
  expect_equal(k$exp, c(1.5, 1.5))
  expect_equal(k$z, round(sqrt(unname(ref$statistic)), 4))
  expect_equal(k$sig, round(ref$p.value, 4))

  # nearby case: missing value on the failure side, all remaining successes
  d2 <- data.frame(x = factor(c(NA, "yes", "yes"), levels = c("no", "yes")))
  k2 <- ifr_os_prop_test(d2, x)
  expect_equal(k2$n, 2)
  expect_equal(k2$phat, 1)
  expect_equal(k2$obs, c(0, 2))
})
