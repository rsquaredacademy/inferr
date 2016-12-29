context('prop-test')

test_that('output from prop_test matches expected result', {

    k <- prop_test(200, 0.3, prob = 0.5, alternative = 'both')
    expect_equal(k$n, 200)
    expect_equal(k$phat, 0.3)
    expect_equal(k$p, 0.5)
    expect_equal(k$z, -5.6569)
    expect_equal(k$sig, 0)
    expect_equivalent(k$alt, 'both')
    expect_equivalent(k$obs, c(140, 60))
    expect_equivalent(k$exp, c(100, 100))
    expect_equivalent(k$deviation, c(' 40.00', '-40.00'))
    expect_equivalent(k$std, c(' 4.00', '-4.00'))

})

test_that('output from prop_test matches expected result', {

    k <- prop_test(200, 0.3, prob = 0.5, alternative = 'less')
    expect_equal(k$sig, 0)
    expect_equivalent(k$alt, 'less')

})

test_that('output from prop_test matches expected result', {

    k <- prop_test(200, 0.3, prob = 0.5, alternative = 'greater')
    expect_equal(k$sig, 1)
    expect_equivalent(k$alt, 'greater')

})


test_that('output from prop_test matches expected result', {

    k <- prop_test(200, 0.3, prob = 0.5, alternative = 'all')
    expect_equal(unname(k$sig), c(0, 0, 1))
    expect_equivalent(k$alt, 'all')

})



test_that('prop_test throws appropriate errors', {

    expect_error(prop_test('200', 0.3, prob = 0.5),
        'n must be numeric')
    expect_error(prop_test(200, '0.3', prob = 0.5),
                 'phat must be numeric')
    expect_error(prop_test(200, 0.3, prob = '0.5'),
                 'prob must be numeric')
    expect_error(prop_test(200, -0.3, prob = 0.5),
                 'phat must be between 0 and 1')
    expect_error(prop_test(200, -1.3, prob = 0.5),
                 'phat must be between 0 and 1')
    expect_error(prop_test(200, 0.3, prob = -0.5),
                 'prob must be between 0 and 1')
    expect_error(prop_test(200, 0.3, prob = -1.5),
                 'prob must be between 0 and 1')

})


test_that('prop_test throws appropriate errors', {

    expect_error(prop_test(as.factor(hsb$race), prob = 0.5),
                 'Please specify a categorical variable with only 2 levels.')
    expect_error(prop_test(200, 0.3, prob = '0.5'),
                 'prob must be numeric')
    expect_error(prop_test(as.factor(hsb$race), prob = -0.5),
                 'prob must be between 0 and 1')
    expect_error(prop_test(as.factor(hsb$race), prob = -1.5),
                 'prob must be between 0 and 1')

})
