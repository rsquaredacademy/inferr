context('binomtest')

test_that('output from binom_test matches the expected output', {

    k <- binom_test(32, 8)
    expect_equal(k$n, 32)
    expect_equal(k$k, 8)
    expect_equal(k$exp_k, 16)
    expect_equal(k$obs_p, 0.25)
    expect_equal(k$exp_p, 0.5)
    expect_equal(k$ik, 24)
    expect_equal(k$lower, 0.0035)
    expect_equal(k$upper, 0.998949)
    expect_equal(k$two_tail, 0.007)

    k <- binom_test(32, 20)
    expect_equal(k$n, 32)
    expect_equal(k$k, 20)
    expect_equal(k$exp_k, 16)
    expect_equal(k$obs_p, 0.625)
    expect_equal(k$exp_p, 0.5)
    expect_equal(k$ik, 16)
    expect_equal(k$lower, 0.944908)
    expect_equal(k$upper, 0.107664)
    expect_equal(k$two_tail, 0.677639)

})

test_that('binom_test throws the appropriate error', {
    expect_error(binom_test('32', 20), 'n must be an integer')
    expect_error(binom_test(32, '20'), 'success must be an integer')
    expect_error(binom_test(32, 20, '0.5'), 'prob must be numeric')
    expect_error(binom_test(32, 20, 1.5), 'prob must be between 0 and 1')
    expect_error(binom_test(32, 20, -1.5), 'prob must be between 0 and 1')
})


test_that('output from binom_calc matches the expected output', {

    k <- binom_calc(as.factor(mtcars$vs))
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

test_that('binom_calc throws the appropriate error', {
    expect_error(binom_calc(mtcars$cyl), 'data must be of type factor')
    expect_error(binom_calc(as.factor(mtcars$cyl), '0.5'), 'prob must be numeric')
    expect_error(binom_calc(as.factor(mtcars$cyl), 1.5), 'prob must be between 0 and 1')
    expect_error(binom_calc(as.factor(mtcars$cyl), -1.5), 'prob must be between 0 and 1')
})
