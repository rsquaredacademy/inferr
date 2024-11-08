test_that("output from ifr_binom_calc matches the expected output", {
  k <- ifr_binom_calc(32, 8)
  expect_equal(k$n, 32)
  expect_equal(k$k, 8)
  expect_equal(k$exp_k, 16)
  expect_equal(k$obs_p, 0.25)
  expect_equal(k$exp_p, 0.5)
  expect_equal(k$pval_lower, 0.0035)
  expect_equal(k$pval_upper, 0.998949)


  k <- ifr_binom_calc(32, 20)
  expect_equal(k$n, 32)
  expect_equal(k$k, 20)
  expect_equal(k$exp_k, 16)
  expect_equal(k$obs_p, 0.625)
  expect_equal(k$exp_p, 0.5)
  expect_equal(k$pval_lower, 0.944908)
  expect_equal(k$pval_upper, 0.107664)
})

test_that("ifr_binom_calc throws the appropriate error", {
  expect_error(ifr_binom_calc("32", 20), "n must be an integer")
  expect_error(ifr_binom_calc(32, "20"), "success must be an integer")
  expect_error(ifr_binom_calc(32, 20, "0.5"), "prob must be numeric")
  expect_error(ifr_binom_calc(32, 20, 1.5), "prob must be between 0 and 1")
  expect_error(ifr_binom_calc(32, 20, -1.5), "prob must be between 0 and 1")
})


mt <- mtcars
mt$vs <- as.factor(mt$vs)
mt$am <- as.factor(mt$am)

test_that("output from ifr_binom_test matches the expected output", {
  k <- ifr_binom_test(mt, vs)
  expect_equal(k$n, 32)
  expect_equal(k$k, 14)
  expect_equal(k$exp_k, 16)
  expect_equal(k$obs_p, 0.4375)
  expect_equal(k$exp_p, 0.5)
  expect_equal(k$pval_lower, 0.298307)
  expect_equal(k$pval_upper, 0.811457)
})

test_that("ifr_binom_test throws the appropriate error", {
  expect_error(ifr_binom_test(mtcars, mpg), "variable must be of type factor")
  expect_error(ifr_binom_test(mt, am, "0.5"), "prob must be numeric")
  expect_error(ifr_binom_test(mt, am, 1.5), "prob must be between 0 and 1")
  expect_error(ifr_binom_test(mt, am, -1.5), "prob must be between 0 and 1")
})


test_that("output from ifr_binom_calc is as expected when k < exp_k", {
  expect_snapshot(ifr_binom_calc(32, 8))
})

test_that("output from ifr_binom_calc is as expected when k > exp_k", {
  expect_snapshot(ifr_binom_calc(32, 20))
})
