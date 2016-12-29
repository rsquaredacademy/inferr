context('levene-test')

test_that('output from levene_test matches the expected result', {

    k <- levene_test(mtcars$mpg, group_var = mtcars$cyl)
    expect_equal(k$bf, 6.4843)
    expect_equal(k$p_bf, 0.0047)
    expect_equal(k$lev, 5.5071)
    expect_equal(k$p_lev, 0.0094)
    expect_equal(k$bft, 6.2047)
    expect_equal(k$p_bft, 0.0057)
    expect_equivalent(k$avgs, c(26.66, 19.74, 15.10))
    expect_equivalent(k$sds, c(4.51, 1.45, 2.56))
    expect_equal(k$avg, 20.09)
    expect_equal(k$sd, 6.03)
    expect_equal(k$n, 32)
    expect_equivalent(k$levs, c('4', '6', '8'))
    expect_equal(k$n_df, 2)
    expect_equal(k$d_df, 29)
    expect_equivalent(k$lens, c(11, 7, 14))

})

test_that('output from levene_test matches the expected result', {

    k <- levene_test(mtcars$mpg, mtcars$qsec)
    expect_equal(k$bf, 24.3932)
    expect_equal(k$p_bf, 0)
    expect_equal(k$lev, 20.9464)
    expect_equal(k$p_lev, 0)
    expect_equal(k$bft, 22.7064)
    expect_equal(k$p_bft, 0)
    expect_equivalent(k$avgs, c(20.09, 17.85))
    expect_equivalent(k$sds, c(6.03, 1.79))
    expect_equal(k$avg, 18.97)
    expect_equal(k$sd, 4.55)
    expect_equal(k$n, 64)
    expect_equivalent(k$levs, c('1', '2'))
    expect_equal(k$n_df, 1)
    expect_equal(k$d_df, 62)
    expect_equivalent(k$lens, c(32, 32))

})


test_that('levene_test throws appropriate errors', {
    expect_error(levene_test(mtcars$mpg),
                 'Please specify at least two variables.')
    expect_error(levene_test(mtcars$mpg, group_var = mtcars$cyl[-1]),
                 'Length of variable and group_var do not match.')
})
