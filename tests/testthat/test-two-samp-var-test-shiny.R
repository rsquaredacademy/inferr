context('var test shiny')

test_that('output from var_test_shiny matches expected result', {

    k <- var_test_shiny(mtcars, 'mpg', 'qsec', alternative = 'less')
    expect_equal(k$f, 11.3756)
    expect_equal(k$lower, 1)
    expect_equal(k$upper, 0)
    expect_equivalent(as.vector(k$vars), c(36.32, 3.19))
    expect_equivalent(as.vector(k$avgs), c(20.09, 17.85))
    expect_equivalent(as.vector(k$sds), c(6.03, 1.79))
    expect_equivalent(as.vector(k$ses), c(1.07, 0.32))
    expect_equal(k$avg, 18.97)
    expect_equal(k$sd, 4.55)
    expect_equal(k$se, 0.57)
    expect_equal(k$len, 64)
    expect_equivalent(as.vector(k$lens), c(32, 32))
    expect_equal(unname(k$n1), 31)
    expect_equal(unname(k$n2), 31)
    expect_equivalent(k$type, 'less')
    expect_equivalent(k$lev, c('mpg', 'qsec'))

})
