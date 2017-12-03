context('two sample proportion test')

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'less')
    expect_equal(k$n1, 32)
    expect_equal(k$n2, 32)
    expect_equal(k$phat1, 0.4062)
    expect_equal(k$phat2, 0.4375)
    expect_equal(round(k$z, 3), -0.254)
    expect_equal(round(k$sig,3), 0.4)
    expect_equivalent(k$alt, 'less')
})


test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'greater')
    expect_equal(round(k$sig, 3), 0.6)
    expect_equivalent(k$alt, 'greater')
})


test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'both')
    expect_equal(round(k$sig, 3), 0.8)
    expect_equivalent(k$alt, 'both')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs,
                      alternative = 'all')
    expect_equal(unname(round(k$sig, 3)), c(0.8, 0.4, 0.6))
    expect_equivalent(k$alt, 'all')
})


test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'less')
    expect_equal(k$n1, 18)
    expect_equal(k$n2, 14)
    expect_equal(round(k$phat1, 3), 0.333)
    expect_equal(k$phat2, 0.5)
    expect_equal(round(k$z, 3), -0.952)
    expect_equal(round(k$sig, 3), 0.17)
    expect_equivalent(k$alt, 'less')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'greater')
    expect_equal(round(k$sig, 3), 0.83)
    expect_equivalent(k$alt, 'greater')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'both')
    expect_equal(round(k$sig, 3), 0.341)
    expect_equivalent(k$alt, 'both')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_grp(var = mtcars$am, group = mtcars$vs,
                     alternative = 'all')
    expect_equal(unname(round(k$sig, 3)), c(0.341, 0.17, 0.83))
    expect_equivalent(k$alt, 'all')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'less')
    expect_equal(k$n1, 30)
    expect_equal(k$n2, 25)
    expect_equal(round(k$phat1, 3), 0.3)
    expect_equal(k$phat2, 0.5)
    expect_equal(round(k$z, 3), -1.514)
    expect_equal(round(k$sig, 3), 0.065)
    expect_equivalent(k$alt, 'less')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'greater')
    expect_equal(round(k$sig, 3), 0.935)
    expect_equivalent(k$alt, 'greater')
})

test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'both')
    expect_equal(round(k$sig, 3), 0.13)
    expect_equivalent(k$alt, 'both')
})


test_that('output from infer_ts_prop_test matches the expected result', {

    k <- infer_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5,
                      alternative = 'all')
    expect_equal(unname(round(k$sig, 3)), c(0.13, 0.065, 0.935))
    expect_equivalent(k$alt, 'all')
})

test_that('output from 2 sample proportion test is as expected when alternative is less', {

  x <- cat("    Test Statistics
------------------------
Sample Size           91
z                  0.351
Pr(Z < z)          0.637")

  expect_equivalent(print(infer_ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'less')), x)

})

test_that('output from 2 sample proportion test is as expected when alternative is greater', {

  x <- cat("    Test Statistics
------------------------
Sample Size           91
z                  0.351
Pr(Z > z)          0.363")

  expect_equivalent(print(infer_ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'greater')), x)

})

test_that('output from 2 sample proportion test is as expected when alternative is both', {

  x <- cat("    Test Statistics
------------------------
Sample Size           91
z                  0.351
Pr(|Z| > |z|)      0.726")

  expect_equivalent(print(infer_ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'both')), x)

})

test_that('output from 2 sample proportion test is as expected when alternative is all', {

  x <- cat("    Test Statistics
------------------------
Sample Size           91
z                  0.351
Pr(|Z| > |z|)      0.726
Pr(Z < z)          0.637
Pr(Z > z)          0.363")

  expect_equivalent(print(infer_ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'all')), x)

})
