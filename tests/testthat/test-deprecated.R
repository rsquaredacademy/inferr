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

test_that("every deprecated alias warns (M7)", {
  calls <- list(
    quote(infer_oneway_anova(hsb, write, prog)),
    quote(infer_binom_calc(32, 13)),
    quote(infer_binom_test(hsb, female)),
    quote(infer_chisq_assoc_test(hsb, female, schtyp)),
    quote(infer_chisq_gof_test(hsb, race, c(20, 20, 20, 140))),
    quote(infer_cochran_qtest(exam, exam1, exam2, exam3)),
    quote(infer_launch_shiny_app(hsb, write, prog)),
    quote(infer_levene_test(hsb, read, group_var = race)),
    quote(infer_mcnemar_test(matrix(c(1, 2, 3, 4), 2))),
    quote(infer_os_prop_test(200, prob = 0.5, phat = 0.3)),
    quote(infer_os_t_test(mtcars, mpg, mu = 50)),
    quote(infer_os_var_test(mtcars, mpg, 5)),
    quote(infer_runs_test(hsb, read)),
    quote(infer_ts_ind_ttest(hsb, female, write)),
    quote(infer_ts_paired_ttest(hsb, read, write)),
    quote(infer_ts_prop_calc(10, 12, 0.5, 0.6)),
    quote(infer_ts_prop_grp(hsb, female, schtyp)),
    quote(infer_ts_prop_test(hsb$female, hsb$schtyp)),
    quote(infer_ts_var_test(hsb, read, group_var = female))
  )

  for (cl in calls) {
    expect_warning(eval(cl), "deprecated")
  }
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
