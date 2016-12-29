context('cochran')

test_that('output from cochran_test matches the expected result', {
    k <- cochran_test(exam)
    expect_equal(k$n, 15)
    expect_equal(k$df, 2)
    expect_equal(k$q, 4.75)
    expect_equal(k$pvalue, 0.093)
})

test_that('output from cochran_test matches the expected result', {
    k <- cochran_test(exam$exam1, exam$exam2, exam$exam3)
    expect_equal(k$n, 15)
    expect_equal(k$df, 2)
    expect_equal(k$q, 4.75)
    expect_equal(k$pvalue, 0.093)
})


test_that('cochran_test throws appropriate errors', {
    expect_error(cochran_test(exam$exam1, exam$exam2),
                 'Please specify at least 3 variables.')
    expect_error(cochran_test(exam[, c(1, 2)]),
                 'Please specify at least 3 variables.')
    expect_error(cochran_test(exam$exam1, exam$exam2),
                 'Please specify at least 3 variables.')
    expect_error(cochran_test(hsb[, c(2, 3, 5)]),
                 'Please specify dichotomous/binary variables only.')
    expect_error(cochran_test(hsb$female, hsb$schtyp, hsb$race),
                 'Please specify dichotomous/binary variables only.')
})

