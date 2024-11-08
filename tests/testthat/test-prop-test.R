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
