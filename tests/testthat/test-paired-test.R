context('paired t test')

test_that('output from paired_ttest matches expected output', {

    k <- paired_ttest(mtcars$mpg, mtcars$qsec)
    expect_equal(k$Obs, 32)
    expect_equal(unname(k$b[[1]]), c(20.09, 17.85, 2.24))
    expect_equal(unname(k$b[[2]]), c(6.03, 1.79, 5.52))
    expect_equal(unname(k$b[[3]]), c(1.07, 0.32, 0.98))
    expect_equal(unlist(k$conf_int1), c(17.92, 22.26))
    expect_equal(unlist(k$conf_int2), c(17.2, 18.5))
    expect_equal(unlist(k$conf_int_diff), c(0.25, 4.23))
    expect_equal(k$corr, 0.42)
    expect_equal(k$corsig, 0.02)
    expect_equal(unname(k$tstat), 2.2857)
    expect_equal(round(unname(k$p_lower), 3), 0.985)
    expect_equal(round(unname(k$p_upper), 3), 0.015)
    expect_equal(round(unname(k$p_two_tail), 3), 0.029)
    expect_equivalent(k$var_names, c('mpg', 'qsec'))
    expect_equivalent(k$xy, c('mpg - qsec'))
    expect_equivalent(k$alternative, c('both'))
    expect_equal(k$df, 31)
    expect_equal(k$confint, 0.95)

})


test_that('paired_ttest throws appropriate error', {
    expect_error(paired_ttest(as.factor(mtcars$mpg), mtcars$qsec),
                 'x must be numeric')
    expect_error(paired_ttest(mtcars$mpg, as.factor(mtcars$qsec)),
                 'y must be numeric')
    expect_error(paired_ttest(mtcars$mpg, mtcars$qsec, '0.95'),
                 'confint must be numeric')
})
