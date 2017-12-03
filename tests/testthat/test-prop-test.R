context('prop-test')

test_that('output from infer_os_prop_test matches expected result', {

    k <- infer_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = 'both')
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

test_that('output from infer_os_prop_test matches expected result when using factor variables', {

  k <- infer_os_prop_test(as.factor(hsb$female), prob = 0.5)
  expect_equal(k$n, 200)
  expect_equal(k$phat, 0.545)
  expect_equal(k$p, 0.5)
  expect_equal(k$z, 1.2728)
  expect_equal(k$sig, 0.2031)
  expect_equivalent(k$alt, 'both')
  expect_equivalent(k$obs, c(91, 109))
  expect_equivalent(k$exp, c(100, 100))
  expect_equivalent(k$deviation, c('-9.00', ' 9.00'))
  expect_equivalent(k$std, c('-0.90', ' 0.90'))

})

test_that('output from infer_os_prop_test matches expected result', {

    k <- infer_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = 'less')
    expect_equal(k$sig, 0)
    expect_equivalent(k$alt, 'less')

})

test_that('output from infer_os_prop_test matches expected result', {

    k <- infer_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = 'greater')
    expect_equal(k$sig, 1)
    expect_equivalent(k$alt, 'greater')

})


test_that('output from infer_os_prop_test matches expected result', {

    k <- infer_os_prop_test(200, phat = 0.3, prob = 0.5, alternative = 'all')
    expect_equal(unname(k$sig), c(0, 0, 1))
    expect_equivalent(k$alt, 'all')

})



test_that('infer_os_prop_test throws appropriate errors', {

    expect_error(infer_os_prop_test('200', phat = 0.3, prob = 0.5),
        'n must be numeric')
    expect_error(infer_os_prop_test(200, phat = '0.3', prob = 0.5),
                 'phat must be numeric')
    expect_error(infer_os_prop_test(200, phat = 0.3, prob = '0.5'),
                 'prob must be numeric')
    expect_error(infer_os_prop_test(200, phat = -0.3, prob = 0.5),
                 'phat must be between 0 and 1')
    expect_error(infer_os_prop_test(200, phat = -1.3, prob = 0.5),
                 'phat must be between 0 and 1')
    expect_error(infer_os_prop_test(200, phat = 0.3, prob = -0.5),
                 'prob must be between 0 and 1')
    expect_error(infer_os_prop_test(200, phat = 0.3, prob = -1.5),
                 'prob must be between 0 and 1')

})


test_that('infer_os_prop_test throws appropriate errors', {

    expect_error(infer_os_prop_test(as.factor(hsb$race), prob = 0.5),
                 'Please specify a categorical variable with only 2 levels.')
    expect_error(infer_os_prop_test(200, phat = 0.3, prob = '0.5'),
                 'prob must be numeric')
    expect_error(infer_os_prop_test(as.factor(hsb$race), prob = -0.5),
                 'prob must be between 0 and 1')
    expect_error(infer_os_prop_test(as.factor(hsb$race), prob = -1.5),
                 'prob must be between 0 and 1')

})


test_that('output from one sample proportion test is as expected when alternative is less', {

  x <- cat("     Test Statistics
-------------------------
Sample Size           200
Exp Prop              0.5
Obs Prop            0.545
z                  1.2728
Pr(Z < z)          0.8985

-----------------------------------------------------------------
Category    Observed    Expected    % Deviation    Std. Residuals
-----------------------------------------------------------------
   0           91         100          -9.00           -0.90
   1          109         100           9.00            0.90
-----------------------------------------------------------------")

  expect_equivalent(print(infer_os_prop_test(as.factor(hsb$female), prob = 0.5), alternative = 'less'), x)

})

test_that('output from one sample proportion test is as expected when alternative is greater', {

  x <- cat("     Test Statistics
-------------------------
Sample Size           200
Exp Prop              0.5
Obs Prop            0.545
z                  1.2728
Pr(Z > z)          0.1015

-----------------------------------------------------------------
Category    Observed    Expected    % Deviation    Std. Residuals
-----------------------------------------------------------------
   0           91         100          -9.00           -0.90
   1          109         100           9.00            0.90
-----------------------------------------------------------------")

  expect_equivalent(print(infer_os_prop_test(as.factor(hsb$female), prob = 0.5), alternative = 'greater'), x)

})

test_that('output from one sample proportion test is as expected when alternative is both', {

  x <- cat("     Test Statistics
-------------------------
Sample Size           200
Exp Prop              0.5
Obs Prop            0.545
z                  1.2728
Pr(|Z| > |z|)      0.2031

-----------------------------------------------------------------
Category    Observed    Expected    % Deviation    Std. Residuals
-----------------------------------------------------------------
   0           91         100          -9.00           -0.90
   1          109         100           9.00            0.90
-----------------------------------------------------------------")

  expect_equivalent(print(infer_os_prop_test(as.factor(hsb$female), prob = 0.5), alternative = 'both'), x)

})

test_that('output from one sample proportion test is as expected when alternative is all', {

  x <- cat("     Test Statistics
-------------------------
Sample Size           200
Exp Prop              0.5
Obs Prop            0.545
z                  1.2728
Pr(|Z| > |z|)      0.2031
Pr(Z < z)          0.8985
Pr(Z > z)          0.1015

-----------------------------------------------------------------
Category    Observed    Expected    % Deviation    Std. Residuals
-----------------------------------------------------------------
   0           91         100          -9.00           -0.90
   1          109         100           9.00            0.90
-----------------------------------------------------------------")

  expect_equivalent(print(infer_os_prop_test(as.factor(hsb$female), prob = 0.5), alternative = 'all'), x)

})






















