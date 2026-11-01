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

test_that("ifr_cochran_qtest handles missing values consistently across encodings (M5)", {
  dlog <- data.frame(
    a = c(TRUE, FALSE, TRUE),
    b = c(FALSE, TRUE, FALSE),
    c = c(TRUE, TRUE, NA)
  )
  dint <- as.data.frame(lapply(dlog, as.integer))

  klog <- ifr_cochran_qtest(dlog, a, b, c)
  kint <- ifr_cochran_qtest(dint, a, b, c)

  expect_equal(klog$n, 2)
  expect_equal(klog$df, 2)
  expect_equal(klog$q, 1)
  expect_equal(klog$pvalue, round(pchisq(1, 2, lower.tail = FALSE), 4))
  expect_equal(klog$pvalue, 0.6065)
  expect_equal(
    unname(c(klog$n, klog$df, klog$q, klog$pvalue)),
    unname(c(kint$n, kint$df, kint$q, kint$pvalue))
  )

  # complete cases, not column-wise means, define the retained sample
  d2 <- data.frame(a = c(1, 1, 0), b = c(1, 0, 1), c = c(NA, 0, 1))
  expect_equal(ifr_cochran_qtest(d2, a, b, c)$n, 2)
})

test_that("declared factor levels survive homogeneous columns (M9)", {
  d <- data.frame(a = c(0, 0, 0, 0),
                  b = c(0, 1, 0, 1),
                  c = c(1, 0, 1, 0))
  f <- as.data.frame(lapply(d, factor, levels = c(0, 1)))
  l <- as.data.frame(lapply(d, as.logical))

  k  <- ifr_cochran_qtest(d, a, b, c)
  kf <- ifr_cochran_qtest(f, a, b, c)
  kl <- ifr_cochran_qtest(l, a, b, c)

  expect_equal(k$n, 4)
  expect_equal(k$df, 2)
  expect_equal(k$q, 2)
  expect_equal(k$pvalue, round(pchisq(2, 2, lower.tail = FALSE), 4))
  expect_equal(k$pvalue, 0.3679)
  expect_equal(unname(c(kf$n, kf$df, kf$q, kf$pvalue)),
               unname(c(k$n, k$df, k$q, k$pvalue)))
  expect_equal(unname(c(kl$n, kl$df, kl$q, kl$pvalue)),
               unname(c(k$n, k$df, k$q, k$pvalue)))

  # an all-success column must also keep its declared levels
  d1 <- data.frame(a = c(1, 1, 1, 1),
                   b = c(0, 1, 0, 1),
                   c = c(1, 0, 1, 0))
  f1 <- as.data.frame(lapply(d1, factor, levels = c(0, 1)))
  l1 <- as.data.frame(lapply(d1, as.logical))

  k1  <- ifr_cochran_qtest(d1, a, b, c)
  k1f <- ifr_cochran_qtest(f1, a, b, c)
  k1l <- ifr_cochran_qtest(l1, a, b, c)

  expect_equal(k1$q, 2)
  expect_equal(k1f$q, k1$q)
  expect_equal(k1l$q, k1$q)
  expect_equal(k1f$pvalue, k1$pvalue)

  # a column can become homogeneous only after complete-case filtering
  d2 <- data.frame(a = c(0, 1, 0, 0, 0),
                   b = c(0, NA, 1, 0, 1),
                   c = c(1, 0, 0, 1, 0))
  f2 <- as.data.frame(lapply(d2, factor, levels = c(0, 1)))
  l2 <- as.data.frame(lapply(d2, as.logical))

  k2  <- ifr_cochran_qtest(d2, a, b, c)
  k2f <- ifr_cochran_qtest(f2, a, b, c)
  k2l <- ifr_cochran_qtest(l2, a, b, c)

  expect_equal(k2$n, 4)
  expect_equal(k2$q, 2)
  expect_equal(k2f$q, k2$q)
  expect_equal(k2l$q, k2$q)
  expect_equal(k2f$pvalue, k2$pvalue)

  # a third declared level is still not binary
  d3 <- data.frame(a = c(0, 0, 0, 0),
                   b = c(0, 1, 0, 1),
                   c = c(1, 0, 1, 0))
  f3 <- as.data.frame(lapply(d3, factor, levels = c(0, 1, 2)))
  expect_error(ifr_cochran_qtest(f3, a, b, c),
               "Please specify dichotomous/binary variables only.")
})

test_that("a homogeneous 1/2-coded column follows the rest of the design (H13)", {
  d01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 1, 1, 1),
                    c = c(0, 1, 1, 1))
  d12 <- d01 + 1
  f12 <- as.data.frame(lapply(d12, factor, levels = c(1, 2)))

  k01  <- ifr_cochran_qtest(d01, a, b, c)
  k12  <- ifr_cochran_qtest(d12, a, b, c)
  kf12 <- ifr_cochran_qtest(f12, a, b, c)

  expect_equal(k01$n, 4)
  expect_equal(k01$df, 2)
  expect_equal(k01$q, 6)
  expect_equal(k01$pvalue, round(pchisq(6, 2, lower.tail = FALSE), 4))
  expect_equal(k01$pvalue, 0.0498)

  expect_equal(unname(c(k12$n, k12$df, k12$q, k12$pvalue)),
               unname(c(k01$n, k01$df, k01$q, k01$pvalue)))
  expect_equal(unname(c(kf12$n, kf12$df, kf12$q, kf12$pvalue)),
               unname(c(k01$n, k01$df, k01$q, k01$pvalue)))

  # nearby design that also disagreed before the fix
  n01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 0, 0, 1),
                    c = c(0, 0, 1, 1))
  n12 <- n01 + 1
  nf  <- as.data.frame(lapply(n12, factor, levels = c(1, 2)))

  q01 <- ifr_cochran_qtest(n01, a, b, c)
  q12 <- ifr_cochran_qtest(n12, a, b, c)
  qf  <- ifr_cochran_qtest(nf, a, b, c)

  expect_equal(q01$q, 3)
  expect_equal(q01$pvalue, round(pchisq(3, 2, lower.tail = FALSE), 4))
  expect_equal(q01$pvalue, 0.2231)
  expect_equal(unname(c(q12$n, q12$df, q12$q, q12$pvalue)),
               unname(c(q01$n, q01$df, q01$q, q01$pvalue)))
  expect_equal(unname(c(qf$n, qf$df, qf$q, qf$pvalue)),
               unname(c(q01$n, q01$df, q01$q, q01$pvalue)))
})

test_that("all-success and NA-homogenized 1/2 columns keep their coding (H13)", {
  # an all-2 (all-success) column in a 1/2 design maps to all 1s
  s12 <- data.frame(a = c(2, 2, 2, 2),
                    b = c(1, 2, 2, 2),
                    c = c(1, 2, 2, 2))
  s01 <- data.frame(a = c(1, 1, 1, 1),
                    b = c(0, 1, 1, 1),
                    c = c(0, 1, 1, 1))
  sf  <- as.data.frame(lapply(s12, factor, levels = c(1, 2)))

  k12 <- ifr_cochran_qtest(s12, a, b, c)
  k01 <- ifr_cochran_qtest(s01, a, b, c)
  kf  <- ifr_cochran_qtest(sf, a, b, c)

  expect_equal(k01$q, 2)
  expect_equal(k01$pvalue, round(pchisq(2, 2, lower.tail = FALSE), 4))
  expect_equal(k01$pvalue, 0.3679)
  expect_equal(unname(c(k12$n, k12$df, k12$q, k12$pvalue)),
               unname(c(k01$n, k01$df, k01$q, k01$pvalue)))
  expect_equal(unname(c(kf$n, kf$df, kf$q, kf$pvalue)),
               unname(c(k01$n, k01$df, k01$q, k01$pvalue)))

  # a column made homogeneous by missing-data removal keeps the 1/2 coding
  # implied by the retained columns
  d12 <- data.frame(a = c(1, 2, 1, 1, 1),
                    b = c(1, NA, 2, 1, 2),
                    c = c(2, 1, 1, 2, 1))
  d01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 1, 0, 1),
                    c = c(1, 0, 1, 0))

  k12 <- ifr_cochran_qtest(d12, a, b, c)
  k01 <- ifr_cochran_qtest(d01, a, b, c)

  expect_equal(k12$n, 4)
  expect_equal(k12$q, 2)
  expect_equal(k12$pvalue, 0.3679)
  expect_equal(unname(c(k12$n, k12$df, k12$q, k12$pvalue)),
               unname(c(k01$n, k01$df, k01$q, k01$pvalue)))
})

test_that("mixed 0/1 and 1/2 numeric codings are rejected (H13)", {
  d <- data.frame(a = c(0, 1, 0, 1),
                  b = c(1, 2, 1, 2),
                  c = c(0, 1, 1, 1))
  expect_error(ifr_cochran_qtest(d, a, b, c),
               "single 0/1 or 1/2 coding")
})

test_that("mixed numeric/factor columns share one success coding (H13)", {
  d01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 1, 1, 1),
                    c = c(0, 1, 1, 1))
  d12 <- d01 + 1
  f12 <- as.data.frame(lapply(d12, factor, levels = c(1, 2)))

  mixed <- d12
  mixed$b <- factor(mixed$b, levels = c(1, 2))
  mixed$c <- factor(mixed$c, levels = c(1, 2))

  ref_q <- 6
  ref_p <- round(pchisq(6, 2, lower.tail = FALSE), 4)

  for (d in list(d01, d12, f12, mixed)) {
    k <- ifr_cochran_qtest(d, a, b, c)
    expect_equal(k$n, 4)
    expect_equal(k$df, 2)
    expect_equal(k$q, ref_q)
    expect_equal(k$pvalue, ref_p)
  }
  expect_equal(ref_p, 0.0498)

  # the nearby design that also disagreed before the fix
  n01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 0, 0, 1),
                    c = c(0, 0, 1, 1))
  n12 <- n01 + 1
  nmix <- n12
  nmix$b <- factor(nmix$b, levels = c(1, 2))
  nmix$c <- factor(nmix$c, levels = c(1, 2))

  kn <- ifr_cochran_qtest(nmix, a, b, c)
  expect_equal(kn$q, 3)
  expect_equal(kn$pvalue, round(pchisq(3, 2, lower.tail = FALSE), 4))
  expect_equal(kn$pvalue, 0.2231)

  # coding evidence must be read before complete-case filtering erases it
  pf <- d12
  pf[5, ] <- c(2, NA, 1)
  pf$b <- factor(pf$b, levels = c(1, 2))
  pf$c <- factor(pf$c, levels = c(1, 2))

  kp <- ifr_cochran_qtest(pf, a, b, c)
  expect_equal(kp$n, 4)
  expect_equal(kp$q, ref_q)
  expect_equal(kp$pvalue, ref_p)
})

test_that("every equivalent mixed encoding of the H13 design agrees", {
  d01 <- data.frame(a = c(0, 0, 0, 0),
                    b = c(0, 1, 1, 1),
                    c = c(0, 1, 1, 1))
  d12 <- d01 + 1
  f12 <- as.data.frame(lapply(d12, factor, levels = c(1, 2)))
  l12 <- as.data.frame(lapply(d01, as.logical))

  opts <- list(n = d12, f = f12, l = l12)

  ref_q <- 6
  ref_p <- round(pchisq(6, 2, lower.tail = FALSE), 4)

  grid <- expand.grid(a = c("n", "f", "l"),
                      b = c("n", "f", "l"),
                      c = c("n", "f", "l"),
                      stringsAsFactors = FALSE)

  for (i in seq_len(nrow(grid))) {
    idx <- unlist(grid[i, ])
    d <- data.frame(a = opts[[idx[["a"]]]]$a,
                    b = opts[[idx[["b"]]]]$b,
                    c = opts[[idx[["c"]]]]$c)

    # numeric/logical/logical leaves the homogeneous numeric column unresolved
    if (identical(unname(idx), c("n", "l", "l"))) {
      expect_error(ifr_cochran_qtest(d, a, b, c),
                   "homogeneous numeric column")
    } else {
      k <- ifr_cochran_qtest(d, a, b, c)
      expect_equal(k$q, ref_q)
      expect_equal(k$pvalue, ref_p)
    }
  }
})

test_that("Cochran Q rejects designs without within-subject discordance (M5)", {
  d <- data.frame(a = c(1, 0, 1, 0),
                  b = c(1, 0, 1, 0),
                  c = c(1, 0, 1, 0))
  expect_error(
    ifr_cochran_qtest(d, a, b, c),
    "no within-subject discordance"
  )

  # the same design encoded as a factor also has an undefined statistic
  f <- as.data.frame(lapply(d, factor, levels = c(0, 1)))
  expect_error(
    ifr_cochran_qtest(f, a, b, c),
    "no within-subject discordance"
  )

  # a single discordant observation is estimable
  d2 <- d
  d2$c[1] <- 0
  k <- ifr_cochran_qtest(d2, a, b, c)
  m <- as.matrix(d2)
  q <- (ncol(m) - 1) * (ncol(m) * sum(colSums(m) ^ 2) - sum(m) ^ 2) /
       (ncol(m) * sum(m) - sum(rowSums(m) ^ 2))

  expect_equal(k$n, 4)
  expect_equal(k$df, 2)
  expect_equal(k$q, q)
  expect_equal(k$pvalue, round(pchisq(k$q, k$df, lower.tail = FALSE), 4))
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
