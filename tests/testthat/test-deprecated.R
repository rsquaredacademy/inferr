mtcarz <- mtcars
mtcarz$vs <- as.factor(mtcarz$vs)
mtcarz$am <- as.factor(mtcarz$am)

test_that("deprecated aliases warn instead of computing a result (M7)", {
  expect_warning(infer_os_t_test(mtcars, mpg, mu = 50), "deprecated")
  expect_warning(infer_ts_prop_test(mtcarz, am, vs), "deprecated")
  expect_warning(infer_chisq_assoc_test(hsb, female, schtyp), "deprecated")
  expect_warning(infer_mcnemar_test(matrix(c(1, 2, 3, 4), 2)), "deprecated")
  expect_warning(infer_binom_calc(32, 8), "deprecated")
})

test_that("the launcher reports its soft deprecation before checking suggests (M7)", {
  local_mocked_bindings(
    check_suggests = function(pkg) stop("suggests checked", call. = FALSE),
    .package = "inferr"
  )

  expect_message(
    expect_error(ifr_launch_shiny_app(), "suggests checked"),
    "soft-deprecated"
  )
})
