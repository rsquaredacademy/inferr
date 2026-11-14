test_that("package attach does not call the network", {
  expect_false(
    exists(".onAttach", envir = asNamespace("inferr"), inherits = FALSE)
  )
})
