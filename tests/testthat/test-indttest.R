test_that("output from ifr_ts_ind_ttest matches expected result", {
  k <- ifr_ts_ind_ttest(hsb, female, write)
  expect_equal(k$levels, c("0", "1"))
  expect_equal(as.numeric(k$obs), c(91, 109))
  expect_equal(k$n, 200)
  expect_equal(as.numeric(k$mean), c(50.121, 54.991))
  expect_equal(as.numeric(k$sd), c(10.305, 8.134))
  expect_equal(as.numeric(k$se), c(1.080, 0.779))
  expect_equal(round(as.numeric(k$lower), 3), c(47.975, 53.447))
  expect_equal(round(as.numeric(k$upper), 3), c(52.267, 56.535))
  expect_equal(round(unlist(k$combined), 3), c(
    200.000, 52.775, 9.479, 0.670,
    199.000, 1.972, 51.454, 54.096
  ))
  expect_equal(k$mean_diff, -4.87)
  expect_equal(k$sd_dif, 9.231)
  expect_equal(k$se_dif, 1.304)
  expect_equal(round(k$conf_diff, 3), c(-7.426, -2.314))
  expect_equal(k$df_pooled, 198)
  expect_equal(k$df_satterthwaite, 170)
  expect_equal(round(k$t_pooled, 2), -3.73)
  expect_equal(round(k$t_satterthwaite, 2), -3.66)
  expect_equal(k$sig_pooled_l, 1e-04)
  expect_equal(k$sig_pooled_u, 0.9999)
  expect_equal(k$sig_pooled, 2e-04)
  expect_equal(k$sig, 3e-04)
  expect_equal(k$sig_l, 2e-04)
  expect_equal(k$sig_u, 0.9998)
  expect_equal(k$num_df, 90)
  expect_equal(k$den_df, 108)
  expect_equal(k$f, 1.605)
  expect_equal(k$f_sig, 0.0188)
  expect_equal(k$confint, 0.95)
  expect_equal(k$var_y, "write")
  expect_equal(k$alternative, "both")
})


test_that("output from independent sample t test is as expected when alternative is less", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "less"))
})

test_that("output from independent sample t test is as expected when alternative is greater", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "greater"))
})

test_that("output from independent sample t test is as expected when alternative is both", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "both"))
})

test_that("output from independent sample t test is as expected when alternative is all", {
  expect_snapshot(ifr_ts_ind_ttest(hsb, female, write, alternative = "all"))
})
