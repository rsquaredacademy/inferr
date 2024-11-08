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
