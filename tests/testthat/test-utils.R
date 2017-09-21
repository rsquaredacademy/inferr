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
  expect_equivalent(fk(3, 10), "     3.000")
  expect_equivalent(fw(3, 10), "    3     ")
  expect_equivalent(fn(3, 10), "    3     ")
  expect_equivalent(formats(), "    ")
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

test_that('output from lev_metric matches the expected result', {
  j <- lev_metric(hsb$read, as.factor(hsb$female), mean)
  expect_equal(round(j$fstat, 3), 0.454)
  expect_equal(round(j$p, 3), 0.501)

  k <- lev_metric(hsb$read, as.factor(hsb$female), median)
  expect_equal(round(k$fstat, 3), 0.602)
  expect_equal(round(k$p, 3), 0.439)

  l <- lev_metric(hsb$read, as.factor(hsb$female), mean, trim.mean = 0.1)
  expect_equal(round(l$fstat, 3), 0.454)
  expect_equal(round(l$p, 3), 0.501)
})

test_that('output from serr matches the expected result', {
  k <- mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
	expected <- sum(rowSums(k$tbl) * colSums(k$tbl)) / (sum(k$tbl) ^ 2)
  expect_equal(round(serr(k$tbl, k$kappa, expected), 3), 0.075)
})

test_that('output from tibble_stats matches the expected result', {
  k <- tibble_stats(mtcars, 'mpg', 'cyl')
  expect_equivalent(k[[1]], c(4, 6, 8))
  expect_equivalent(k[[2]], c(11, 7, 14))
  expect_equivalent(round(k[[3]], 3), c(26.664, 19.743, 15.100))
  expect_equivalent(round(k[[4]], 3), c(20.339, 2.113, 6.554))
  expect_equivalent(round(k[[5]], 3), c(4.510, 1.454, 2.560))
  expect_equivalent(round(k[[6]], 3), c(1.360, 0.549, 0.684))
})

test_that('output from tbl_stats matches the expected result', {
  k <- tbl_stats(mtcars, 'mpg')
  expect_equal(k[[1]], 32)
  expect_equal(round(k[[2]], 3), 20.091)
  expect_equal(round(k[[3]], 3), 6.027)
  expect_equal(round(k[[4]], 3), 1.065)
})

test_that('output from paired_data and paired_stats matches the expected result', {
  x <- c(2, 4, 6, 8, 10)
  y <- c(1, 3, 5, 7, 9)
  z <- paired_data(x, y)
  key <- c(rep('x', 5), rep('y', 5), rep('z', 5))
  value <- c(c(2, 4, 6, 8, 10), c(1, 3, 5, 7, 9), rep(1, 5))
  kv <- tibble(key, value)
  expect_equal(z, kv)

  j <- paired_data(hsb$read, hsb$write)
  k <- paired_stats(j, 'key', 'value')
  expect_equivalent(k[[1]], c(52.23, 52.77, -0.55))
  expect_equivalent(k[[2]], c(10.25, 9.48, 8.89))
  expect_equivalent(k[[3]], c(0.72, 0.67, 0.63))
})

test_that('output from cor_sig matches the expected output', {
    expect_equal(cor_sig(cor(hsb$read, hsb$write), length(hsb$read)), 0)
})

test_that('output from conf_int_t matches the expected output', {
    j <- paired_data(hsb$read, hsb$write)
    b <- paired_stats(j, 'key', 'value')
    n <- length(hsb$read)
    expect_equivalent(round(conf_int_t(b[[1, 1]], b[[2, 1]], n, alpha = 0.025), 2), c(43.80, 60.66))
})

test_that('output from data_split matches the expected result', {
  k <- da(mtcars, 'mpg')
  expect_equal(k[[1]], 32)
  expect_equal(k[[2]], 20.091)
  expect_equal(k[[3]], 6.027)
  expect_equal(k[[4]], 1.065)
})

test_that('output from sd_diff and se_diff matches the expected result', {
    expect_equal(round(sd_diff(200, 200, sd(hsb$read), sd(hsb$write)), 3), 3.149)

    n1 <- length(hsb$read)
    n2 <- length(hsb$write)
    s1 <- sd(hsb$read) / sqrt(n1)
    s2 <- sd(hsb$write) / sqrt(n2)
    expect_equal(round(se_diff(n1, n2, s1, s2), 3), 0.084)
})

test_that('output from df matches the expected result', {
    expect_equal(df(10, 15, 3, 6), 23)
})

test_that('output from conf_int_p matches the expected result', {
    expect_equivalent(round(conf_int_p(10, 3.6), 3), c(2.944, 17.056))
})

test_that('output from sums and coch matches the expected result', {
    data <- data.frame(x = c(0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0),
     y = c(1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0),
     z = c(1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1))
    k <- sums(data)
    expect_equivalent(k[[1]], c(x = 10, y = 13, z = 13))
    expect_equal(k[[2]], 438)
    expect_equivalent(k[[3]], c(2, 1, 3, 2, 3, 3, 2, 0, 3, 1, 1, 1, 2, 2, 1, 3, 3, 1, 1, 1))
    expect_equal(k[[4]], 82)

    q <- coch(ncol(data), k$cls_sum, k$cl, k$g, k$gs_sum)
    expect_equal(round(q, 3), 1.385)
})

