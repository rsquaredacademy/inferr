context('binomtest')

test_that('output from infer_binom_calc matches the expected output', {

    k <- infer_binom_calc(32, 8)
    expect_equal(k$n, 32)
    expect_equal(k$k, 8)
    expect_equal(k$exp_k, 16)
    expect_equal(k$obs_p, 0.25)
    expect_equal(k$exp_p, 0.5)
    expect_equal(k$ik, 24)
    expect_equal(k$lower, 0.0035)
    expect_equal(k$upper, 0.998949)
    expect_equal(k$two_tail, 0.007)

    k <- infer_binom_calc(32, 20)
    expect_equal(k$n, 32)
    expect_equal(k$k, 20)
    expect_equal(k$exp_k, 16)
    expect_equal(k$obs_p, 0.625)
    expect_equal(k$exp_p, 0.5)
    expect_equal(k$ik, 11)
    expect_equal(k$lower, 0.944908)
    expect_equal(k$upper, 0.107664)
    expect_equal(k$two_tail, 0.162756)

})

test_that('infer_binom_calc throws the appropriate error', {
    expect_error(infer_binom_calc('32', 20), 'n must be an integer')
    expect_error(infer_binom_calc(32, '20'), 'success must be an integer')
    expect_error(infer_binom_calc(32, 20, '0.5'), 'prob must be numeric')
    expect_error(infer_binom_calc(32, 20, 1.5), 'prob must be between 0 and 1')
    expect_error(infer_binom_calc(32, 20, -1.5), 'prob must be between 0 and 1')
})


test_that('output from infer_binom_test matches the expected output', {

    k <- infer_binom_test(as.factor(mtcars$vs))
    expect_equal(k$n, 32)
    expect_equal(k$k, 14)
    expect_equal(k$exp_k, 16)
    expect_equal(k$obs_p, 0.4375)
    expect_equal(k$exp_p, 0.5)
    expect_equal(k$ik, 18)
    expect_equal(k$lower, 0.298307)
    expect_equal(k$upper, 0.811457)
    expect_equal(k$two_tail, 0.596615)

})

test_that('infer_binom_test throws the appropriate error', {
    expect_error(infer_binom_test(mtcars$cyl), 'data must be of type factor')
    expect_error(infer_binom_test(as.factor(mtcars$am), '0.5'), 'prob must be numeric')
    expect_error(infer_binom_test(as.factor(mtcars$am), 1.5), 'prob must be between 0 and 1')
    expect_error(infer_binom_test(as.factor(mtcars$am), -1.5), 'prob must be between 0 and 1')
})


test_that('output from infer_binom_calc is as expected when k < exp_k', {

  x <- cat("            Binomial Test
 --------------------------------------
  Group    N     Obs. Prop    Exp. Prop
 --------------------------------------
    0      24         0.75        0.500
    1       8         0.25        0.500
 --------------------------------------


                Test Summary
 -------------------------------------------
  Tail             Prob             p-value
 -------------------------------------------
  Lower    Pr(k <= 8)                 0.0035
  Upper    Pr(k >= 8)               0.998949
  Two      Pr(k <= 8 or k >= 24)       0.007
 -------------------------------------------")

  expect_equivalent(print(infer_binom_calc(32, 8)), x)

})

test_that('output from infer_binom_calc is as expected when k > exp_k', {

  x <- cat("            Binomial Test
 --------------------------------------
  Group    N     Obs. Prop    Exp. Prop
 --------------------------------------
    0      12        0.375        0.500
    1      20        0.625        0.500
 --------------------------------------


                 Test Summary
 --------------------------------------------
  Tail              Prob             p-value
 --------------------------------------------
  Lower    Pr(k <= 20)               0.944908
  Upper    Pr(k >= 20)               0.107664
  Two      Pr(k <= 11 or k >= 20)    0.162756
 --------------------------------------------")

  expect_equivalent(print(infer_binom_calc(32, 20)), x)

})
