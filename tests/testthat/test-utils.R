context('utils')

test_that('output from anova_split matches the expected result', {
  smean <- anova_avg(mtcars, 'mpg')
  k <- anova_split(mtcars, 'mpg', 'cyl', smean)
  expect_equivalent(k$cyl, c(4, 6, 8))
  expect_equivalent(k$length, c(11, 7, 14))
  expect_equivalent(round(k$mean, 2), c(26.66, 19.74, 15.100))
  expect_equivalent(round(k$var, 2), c(20.34, 2.11, 6.55))
  expect_equivalent(round(k$sd, 2), c(4.51, 1.45, 2.56))
  expect_equivalent(round(k$sst, 2), c(475.25, 0.85, 348.69))
  expect_equivalent(round(k$sse, 2), c(203.39, 12.68, 85.2))
})

test_that('output from anova_avg matches the expected result', {
  k <- anova_avg(mtcars, 'mpg')
  expect_equal(round(k, 1), 20.1)
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
  expect_equivalent(formatter_pair(3, 6), " 3.00 ")
})

test_that('output from l matches the expected result', {

    expect_equal(l(deparse(substitute(mtcars$mpg))), "mpg")
    expect_equal(l(deparse(substitute(mpg))), "mpg")
    expect_equal(l(deparse(substitute(mtcars@mpg))), "mtcars@mpg")

})

test_that('output from extract matches the expected result', {
  x <- c(1, 2, 3)
  y <- c(3, 2, 1)
  actual <- extract(x, y)
  expected <- tibble(x = as.numeric(1:3), y = as.numeric(3:1), z = c(-2, 0, 2))
  expect_equivalent(actual, expected)
})
