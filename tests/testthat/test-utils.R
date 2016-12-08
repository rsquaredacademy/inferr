context('utils')

test_that('output from anova_split matches the expected result', {
  k <- anova_split(mtcars, 'mpg', 'cyl')
  expect_equivalent(k$cyl, c(4, 6, 8))
  expect_equivalent(k$length, c(11, 7, 14))
  expect_equivalent(round(k$mean, 2), c(26.66, 19.74, 15.100))
  expect_equivalent(round(k$var, 2), c(20.34, 2.11, 6.55))
  expect_equivalent(round(k$sd, 2), c(4.51, 1.45, 2.56))
})

test_that('output from anova_avg matches the expected result', {
  k <- anova_avg(mtcars, 'cyl')
  expect_equal(k$cyl, 6.1875)
})

test_that('output from text formatting matches the expected result', {
  expect_equivalent(fg(3, 10),  "    3     ")
  expect_equivalent(fs(),  "  ")
  expect_equivalent(fl(3, 10),  "3         ")
  expect_equivalent(fc(3, 10),  "    3     ")
  expect_equivalent(formatter_t(3, 10),  "    3     ")
  expect_equivalent(format_cil(3, 10),  "    3     ")
  expect_equivalent(format_ciu(3, 10),  "    3     ")
  expect_equivalent(formats_t(),  "  ")
})
