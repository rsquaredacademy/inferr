context('two sample proportion test')

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'less')
    expect_equal(k$n1, 32)
    expect_equal(k$n2, 32)
    expect_equal(k$phat1, 0.4062)
    expect_equal(k$phat2, 0.4375)
    expect_equal(k$z, -0.2535)
    expect_equal(k$sig, 0.3999)
    expect_equivalent(k$alt, 'less')
})


test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'greater')
    expect_equal(k$sig, 0.6001)
    expect_equivalent(k$alt, 'greater')
})


test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'both')
    expect_equal(k$sig, 0.7999)
    expect_equivalent(k$alt, 'both')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'all')
    expect_equal(unname(k$sig), c(0.7999, 0.3999, 0.6001))
    expect_equivalent(k$alt, 'all')
})


test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'less')
    expect_equal(k$n1, 18)
    expect_equal(k$n2, 14)
    expect_equal(round(k$phat1, 3), 0.333)
    expect_equal(k$phat2, 0.5)
    expect_equal(round(k$z, 3), -0.952)
    expect_equal(round(k$sig, 3), 0.17)
    expect_equivalent(k$alt, 'less')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'greater')
    expect_equal(round(k$sig, 3), 0.83)
    expect_equivalent(k$alt, 'greater')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'both')
    expect_equal(round(k$sig, 3), 0.341)
    expect_equivalent(k$alt, 'both')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'all')
    expect_equal(unname(round(k$sig, 3)), c(0.341, 0.17, 0.83))
    expect_equivalent(k$alt, 'all')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'less')
    expect_equal(k$n1, 30)
    expect_equal(k$n2, 25)
    expect_equal(round(k$phat1, 3), 0.3)
    expect_equal(k$phat2, 0.5)
    expect_equal(round(k$z, 3), -1.514)
    expect_equal(round(k$sig, 3), 0.065)
    expect_equivalent(k$alt, 'less')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'greater')
    expect_equal(round(k$sig, 3), 0.935)
    expect_equivalent(k$alt, 'greater')
})

test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'both')
    expect_equal(round(k$sig, 3), 0.13)
    expect_equivalent(k$alt, 'both')
})


test_that('output from ts_prop_test matches the expected result', {

    k <- ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'all')
    expect_equal(unname(round(k$sig, 3)), c(0.13, 0.065, 0.935))
    expect_equivalent(k$alt, 'all')
})
