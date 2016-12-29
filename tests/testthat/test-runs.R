context('runs test')

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    k <- runs_test(residuals(reg))
    expect_equal(k$n, 32)
    expect_equal(round(k$threshold, 3), -0.963)
    expect_equal(k$n_above, 16)
    expect_equal(k$n_below, 16)
    expect_equal(k$mean, 17)
    expect_equal(round(k$var, 3), 7.742)
    expect_equal(k$n_runs, 11)
    expect_equal(round(k$z, 3), -2.156)
    expect_equal(round(k$p, 3), 0.031)

})

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    k <- runs_test(residuals(reg), drop = TRUE)
    expect_equal(k$n, 32)
    expect_equal(round(k$threshold, 3), -0.963)
    expect_equal(k$n_above, 16)
    expect_equal(k$n_below, 16)
    expect_equal(k$mean, 17)
    expect_equal(round(k$var, 3), 7.742)
    expect_equal(k$n_runs, 11)
    expect_equal(round(k$z, 3), -2.156)
    expect_equal(round(k$p, 3), 0.031)

})

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    k <- runs_test(residuals(reg), split = TRUE)
    expect_equal(k$n, 32)
    expect_equal(round(k$threshold, 3), -0.963)
    expect_equal(k$n_above, 16)
    expect_equal(k$n_below, 16)
    expect_equal(k$mean, 17)
    expect_equal(round(k$var, 3), 7.742)
    expect_equal(k$n_runs, 11)
    expect_equal(round(k$z, 3), -2.156)
    expect_equal(round(k$p, 3), 0.031)

})

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    k <- runs_test(residuals(reg), mean = TRUE)
    expect_equal(k$n, 32)
    expect_equal(round(k$threshold, 3), 1)
    expect_equal(k$n_above, 9)
    expect_equal(k$n_below, 23)
    expect_equal(k$mean, 13.9375)
    expect_equal(round(k$var, 3), 4.982)
    expect_equal(k$n_runs, 9)
    expect_equal(round(k$z, 3), -2.212)
    expect_equal(round(k$p, 3), 0.027)

})

test_that('output from runs test matches the expected result', {

    reg <- lm(mpg ~ disp, data = mtcars)
    k <- runs_test(residuals(reg), threshold = 0)
    expect_equal(k$n, 32)
    expect_equal(round(k$threshold, 3), 0)
    expect_equal(k$n_above, 13)
    expect_equal(k$n_below, 19)
    expect_equal(k$mean, 16.4375)
    expect_equal(round(k$var, 3), 7.19)
    expect_equal(k$n_runs, 11)
    expect_equal(round(k$z, 3), -2.028)
    expect_equal(round(k$p, 3), 0.043)

})


test_that('runs_test throws appropriate errors', {

    reg <- lm(mpg ~ disp, data = mtcars)
    expect_error(runs_test(as.factor(residuals(reg))),
                 'x must be numeric or integer')
    expect_error(runs_test(hsb$female),
                 'Use 0 as threshold if the data is coded as a binary.')

})
