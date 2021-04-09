context("cochran")

test_that("output from infer_cochran_qtest matches the expected result", {
  k <- infer_cochran_qtest(exam, exam1, exam2, exam3)
  expect_equal(k$n, 15)
  expect_equal(k$df, 2)
  expect_equal(k$q, 4.75)
  expect_equal(k$pvalue, 0.093)
})


test_that("infer_cochran_qtest throws appropriate errors", {
  expect_error(
    infer_cochran_qtest(exam, exam1, exam2),
    "Please specify at least 3 variables."
  )
  expect_error(
    infer_cochran_qtest(hsb, female, schtyp, race),
    "Please specify dichotomous/binary variables only."
  )
})

test_that("output from cochran test is as expected", {
  x <- cat("   Test Statistics
----------------------
N                   15
Cochran's Q       4.75
df                   2
p value          0.093
----------------------")

  expect_equivalent(print(infer_cochran_qtest(exam, exam1, exam2, exam3)), x)
})