context('os-vartest')

test_that('output from os_vartest matches the expected result', {

    k <- os_vartest(mtcars$mpg, 0.3)
    expect_equal(k$n, 32)
    expect_equal(k$sd, 0.3)
    expect_equal(k$sigma, 6.0269)
    expect_equal(k$se, 1.0654)
    expect_equal(k$chi, 12511.4359)
    expect_equal(k$df, 31)
    expect_equal(k$p_lower, 1)
    expect_equal(k$p_upper, 0)
    expect_equal(k$p_two, 0)
    expect_equal(k$xbar, 20.0906)
    expect_equal(k$c_lwr, 3.8737)
    expect_equal(k$c_upr, 10.6526)
    expect_equal(k$conf, 0.95)
    expect_equivalent(k$var_name, 'mpg')
    expect_equivalent(k$type, 'both')

})


test_that('os_vartest returns appropriate errors', {
    expect_error(os_vartest('mtcars$mpg', 0.3),
                 'x must be numeric')
    expect_error(os_vartest(mtcars$mpg, '0.3'),
                 'sd must be numeric')
    expect_error(os_vartest(mtcars$mpg, 0.3, '0.95'),
                 'confint must be numeric')
})
