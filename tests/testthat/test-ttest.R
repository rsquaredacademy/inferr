context('ttest')

test_that('output from ttest matches the expected output', {

    k <- ttest(mtcars$mpg, mu = 50, type = 'less')
    expect_equal(k$mu, 50)
    expect_equal(k$n, 32)
    expect_equal(k$df, 31)
    expect_equal(k$Mean, 20.0906)
    expect_equal(k$stddev, 6.0269)
    expect_equal(k$std_err, 1.0654)
    expect_equal(k$test_stat, -28.073)
    expect_equivalent(k$confint, c(-Inf, 21.8974))
    expect_equal(k$mean_diff_l, -Inf)
    expect_equal(k$mean_diff_u, -28.1026)
    expect_equal(k$mean_diff, -29.9094)
    expect_equal(k$p_l, 6.592161e-24)
    expect_equal(k$p_u, 1)
    expect_equal(k$p, 2)
    expect_equal(k$conf, 0.95)
    expect_equivalent(k$type, 'less')
    expect_equivalent(k$var_name, 'mpg')

    k <- ttest(mtcars$mpg, mu = 50, type = 'greater')
    expect_equivalent(k$confint, c(18.2846, Inf))

    k <- ttest(mtcars$mpg, mu = 50, type = 'both')
    expect_equivalent(k$confint, c(17.9181, 22.2639))

})

test_that('ttest throws the appropriate error', {
    expect_error(ttest(as.factor(mtcars$mpg), mu = 50), 'x must be numeric')
    expect_error(ttest(mtcars$mpg, mu = '50'), 'mu must be numeric')
    expect_error(ttest(mtcars$mpg, mu = 50, alpha = '0.05'), 'alpha must be numeric')
})
