context('os-chisq-gof')

test_that('output from os_chisqgof matches the expected result', {

    k <- os_chisqgof(hsb$female, c(100, 100))
    expect_equal(k$chisquare, 1.62)
    expect_equal(k$pvalue, 0.2031)
    expect_equal(k$df, 1)
    expect_equal(k$ssize, 200)
    expect_equivalent(k$names, c('0', '1'))
    expect_equal(k$level, 2)
    expect_equivalent(k$obs, c(91, 109))
    expect_equivalent(k$exp, c(100, 100))
    expect_equivalent(k$deviation, c('-9.00', ' 9.00'))
    expect_equivalent(k$std, c('-0.90', ' 0.90'))
    expect_equivalent(k$varname, 'female')
    expect_equal(round(k$c_low, 3), 0.476)
    expect_equal(round(k$c_up, 3), 0.614)

})


test_that('output from os_chisqgof matches the expected result', {

    k <- os_chisqgof(hsb$female, c(0.5, 0.5))
    expect_equal(k$chisquare, 1.62)
    expect_equal(k$pvalue, 0.2031)
    expect_equal(k$df, 1)
    expect_equal(k$ssize, 200)
    expect_equivalent(k$names, c('0', '1'))
    expect_equal(k$level, 2)
    expect_equivalent(k$obs, c(91, 109))
    expect_equivalent(k$exp, c(100, 100))
    expect_equivalent(k$deviation, c('-9.00', ' 9.00'))
    expect_equivalent(k$std, c('-0.90', ' 0.90'))
    expect_equivalent(k$varname, 'female')
    expect_equal(round(k$c_low, 3), 0.476)
    expect_equal(round(k$c_up, 3), 0.614)

})


test_that('output from os_chisqgof matches the expected result', {

    k <- os_chisqgof(hsb$female, c(0.5, 0.5), correct = TRUE)
    expect_equal(k$chisquare, 1.445)
    expect_equal(k$pvalue, 0.2293)
    expect_equal(k$df, 1)
    expect_equal(k$ssize, 200)
    expect_equivalent(k$names, c('0', '1'))
    expect_equal(k$level, 2)
    expect_equivalent(k$obs, c(91, 109))
    expect_equivalent(k$exp, c(100, 100))
    expect_equivalent(k$deviation, c('-9.50', ' 8.50'))
    expect_equivalent(k$std, c('-0.95', ' 0.85'))
    expect_equivalent(k$varname, 'female')
    expect_equal(round(k$c_low, 3), 0.476)
    expect_equal(round(k$c_up, 3), 0.614)

})

test_that('os_chisqgof throws appropriate errors', {

    expect_warning(os_chisqgof(hsb$race, c(50, 50, 50, 50), correct = TRUE),
                   'Continuity correction is applicable only in case of binary categories.')

})
