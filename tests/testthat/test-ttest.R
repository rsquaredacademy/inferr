context('infer_os_t_test')

test_that('output from infer_os_t_test matches the expected output', {

    k <- infer_os_t_test(mtcars$mpg, mu = 50, type = 'less')
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
    expect_equal(k$p, 1.31843219487798e-23)
    expect_equal(k$conf, 0.95)
    expect_equivalent(k$type, 'less')
    expect_equivalent(k$var_name, 'mpg')

    k <- infer_os_t_test(mtcars$mpg, mu = 50, type = 'greater')
    expect_equivalent(k$confint, c(18.2846, Inf))

    k <- infer_os_t_test(mtcars$mpg, mu = 50, type = 'both')
    expect_equivalent(k$confint, c(17.9181, 22.2639))

})

test_that('infer_os_t_test throws the appropriate error', {
    expect_error(infer_os_t_test(as.factor(mtcars$mpg), mu = 50), 'x must be numeric')
    expect_error(infer_os_t_test(mtcars$mpg, mu = '50'), 'mu must be numeric')
    expect_error(infer_os_t_test(mtcars$mpg, mu = 50, alpha = '0.05'), 'alpha must be numeric')
})

test_that('output from one sample t test is as expected when alternative is less', {

  x <- cat("                              One-Sample Statistics
---------------------------------------------------------------------------------
 Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------------
  write      200    52.775     0.6702       9.4786         -Inf     53.8828
---------------------------------------------------------------------------------

                                 Lower Tail Test
                                 ---------------

                               Ho: mean(write) >=50
                                Ha: mean(write) <50
--------------------------------------------------------------------------------
 Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval]
--------------------------------------------------------------------------------
  write      4.141    199    0.99997       2.775          -Inf      3.8828
--------------------------------------------------------------------------------")

  expect_equivalent(print(infer_os_t_test(hsb$write, mu = 50, type = 'less')), x)

})

test_that('output from one sample t test is as expected when alternative is greater', {

  x <- cat("                              One-Sample Statistics
---------------------------------------------------------------------------------
 Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------------
  write      200    52.775     0.6702       9.4786       51.6678      Inf
---------------------------------------------------------------------------------

                                 Upper Tail Test
                                 ---------------

                               Ho: mean(write) <=50
                               Ha: mean(write) >50
--------------------------------------------------------------------------------
 Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval]
--------------------------------------------------------------------------------
  write      4.141    199    0.99997       2.775         1.6678      Inf
--------------------------------------------------------------------------------")

  expect_equivalent(print(infer_os_t_test(hsb$write, mu = 50, type = 'greater')), x)

})

test_that('output from one sample t test is as expected when alternative is both', {

  x <- cat("                              One-Sample Statistics
---------------------------------------------------------------------------------
 Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------------
  write      200    52.775     0.6702       9.4786       51.4537    54.0969
---------------------------------------------------------------------------------

                                  Two Tail Test
                                 ---------------

                               Ho: mean(write) ~=50
                               Ha: mean(write) !=50
--------------------------------------------------------------------------------
 Variable      t      DF       Sig       Mean Diff.    [95% Conf. Interval]
--------------------------------------------------------------------------------
  write      4.141    199    0.99997       2.775         1.4537     4.0969
--------------------------------------------------------------------------------")

  expect_equivalent(print(infer_os_t_test(hsb$write, mu = 50, type = 'both')), x)

})

test_that('output from one sample t test is as expected when alternative is all', {

  x <- cat("                              One-Sample Statistics
---------------------------------------------------------------------------------
 Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval]
---------------------------------------------------------------------------------
  write      200    52.775     0.6702       9.4786       51.4537    54.0969
---------------------------------------------------------------------------------

                               Ho: mean(write) ~=50

        Ha: mean < 50              Ha: mean ~= 50               Ha: mean > 50
         t = 4.141                   t = 4.141                   t = 4.141
       P < t = 1.0000             P > |t| = 0.0001             P > t = 0.0000")

  expect_equivalent(print(infer_os_t_test(hsb$write, mu = 50, type = 'all')), x)

})
