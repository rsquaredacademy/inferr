test_that("output from ifr_cochran_qtest matches the expected result", {
  k <- ifr_cochran_qtest(exam, exam1, exam2, exam3)
  expect_equal(k$n, 15)
  expect_equal(k$df, 2)
  expect_equal(k$q, 4.75)
  expect_equal(k$pvalue, 0.093)
})


test_that("ifr_cochran_qtest throws appropriate errors", {
  expect_error(
    ifr_cochran_qtest(exam, exam1, exam2),
    "Please specify at least 3 variables."
  )
  expect_error(
    ifr_cochran_qtest(hsb, female, schtyp, race),
    "Please specify dichotomous/binary variables only."
  )
})

test_that("logical columns are recoded to 0/1", {
  d <- within(exam, {
    e1 <- exam1 == "Passed"
    e2 <- exam2 == "Passed"
    e3 <- exam3 == "Passed"
  })

  k  <- ifr_cochran_qtest(d, e1, e2, e3)
  kf <- ifr_cochran_qtest(exam, exam1, exam2, exam3)

  expect_equal(k$q, kf$q)
  expect_equal(k$q, 4.75)
  expect_equal(k$pvalue, 0.093)
})

test_that("binary, logical and factor encodings give the same Q", {
  d01  <- data.frame(a = c(1, 1, 0, 1, 0), b = c(1, 1, 0, 0, 0),
                     c = c(1, 0, 0, 0, 0))
  d12  <- d01 + 1
  dlog <- as.data.frame(lapply(d01, as.logical))
  dfac <- as.data.frame(lapply(d01, factor))

  q <- vapply(
    list(d01, d12, dlog, dfac),
    function(d) ifr_cochran_qtest(d, a, b, c)$q,
    numeric(1)
  )

  expect_equal(unname(q), rep(q[[1]], 4))
})

test_that("non-binary variables are rejected", {
  d <- data.frame(a = 1:4, b = 1:4, c = 1:4)
  expect_error(
    ifr_cochran_qtest(d, a, b, c),
    "Please specify dichotomous/binary variables only."
  )

  d <- data.frame(
    a = c("yes", "no", "yes"),
    b = c("yes", "yes", "no"),
    c = c("no", "no", "yes")
  )
  expect_error(
    ifr_cochran_qtest(d, a, b, c),
    "Please specify dichotomous/binary variables only."
  )
})

test_that("output from cochran test is as expected", {
  expect_snapshot(ifr_cochran_qtest(exam, exam1, exam2, exam3))
})
