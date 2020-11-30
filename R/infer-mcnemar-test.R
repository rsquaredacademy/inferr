#' @importFrom magrittr %>%
#' @title McNemar Test
#' @description Test if the proportions of two dichotomous variables are
#' equal in the same population.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y factor; column in \code{data}
#' @return \code{infer_mcnemar_test} returns an object of class \code{"infer_mcnemar_test"}.
#' An object of class \code{"infer_mcnemar_test"} is a list containing the
#' following components:
#'
#' \item{statistic}{chi square statistic}
#' \item{df}{degrees of freedom}
#' \item{pvalue}{p-value}
#' \item{exactp}{exact p-value}
#' \item{cstat}{continuity correction chi square statistic}
#' \item{cpvalue}{continuity correction p-value}
#' \item{kappa}{kappa coefficient; measure of interrater agreement}
#' \item{std_err}{asymptotic standard error}
#' \item{kappa_cil}{95\% kappa lower confidence limit}
#' \item{kappa_ciu}{95\% kappa upper confidence limit}
#' \item{cases}{cases}
#' \item{controls}{controls}
#' \item{ratio}{ratio of proportion with factor}
#' \item{odratio}{odds ratio}
#' \item{tbl}{two way table}
#' @section Deprecated Function:
#' \code{mcnermar_test()} has been deprecated. Instead use
#' \code{infer_mcnemar_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @seealso \code{\link[stats]{mcnemar.test}}
#' @examples
#' # using variables from data
#' library(dplyr)
#' hb <- mutate(hsb,
#'         himath = if_else(math > 60, 1, 0),
#'         hiread = if_else(read > 60, 1, 0)
#'     )
#' infer_mcnemar_test(hb, himath, hiread)
#'
#' # test if the proportion of students in himath and hiread group is same
#' himath <- ifelse(hsb$math > 60, 1, 0)
#' hiread <- ifelse(hsb$read > 60, 1, 0)
#' infer_mcnemar_test(table(himath, hiread))
#'
#' # using matrix
#' infer_mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
#' @export
#'
infer_mcnemar_test <- function(data, x = NULL, y = NULL) UseMethod("infer_mcnemar_test")

#' @export
#'
infer_mcnemar_test.default <- function(data, x = NULL, y = NULL) {

  if (is.matrix(data) | is.table(data)) {
    dat <- mcdata(data)
  } else {

    x1 <- deparse(substitute(x))
    y1 <- deparse(substitute(y))
    dat <- table(data[c(x1, y1)])

  }

  k <- mccomp(dat)

  result <-
    list(cases     = k$cases,
         controls  = k$controls,
         cpvalue   = k$cpvalue,
         cstat     = k$cstat,
         df        = k$df,
         exactp    = k$exactp,
         kappa     = k$kappa,
         kappa_cil = k$kappa_cil,
         kappa_ciu = k$kappa_ciu,
         odratio   = k$odratio,
         pvalue    = k$pvalue,
         ratio     = k$ratio,
         statistic = k$statistic,
         std_err   = k$std_err,
         tbl       = dat)

  class(result) <- "infer_mcnemar_test"
  return(result)
}

#' @export
#'
print.infer_mcnemar_test <- function(x, ...) {
  print_mcnemar_test(x)
}

mcdata <- function(x, y) {
  if (!is.matrix(x)) {
    stop("x must be either a table or a matrix")
  }

  if (is.matrix(x)) {
    if (length(x) != 4) {
      stop("x must be a 2 x 2 matrix")
    }
  }

  dat <- x
  return(dat)
}


mctestp <- function(dat) {
  retrieve <- matrix(c(1, 2, 2, 1), nrow = 2)
  dat[retrieve]
}

tetat <- function(p) {
  ((p[1] - p[2]) ^ 2) / sum(p)
}

mcpval <- function(test_stat, df) {
  1 - stats::pchisq(test_stat, df)
}

mcpex <- function(dat) {
  2 * min(stats::pbinom(dat[2], sum(dat[2], dat[3]), 0.5), stats::pbinom(dat[3], sum(dat[2], dat[3]), 0.5))
}

mcstat <- function(p) {
  ((abs(p[1] - p[2]) - 1) ^ 2) / sum(p)
}

mccpval <- function(cstat, df) {
  1 - stats::pchisq(cstat, df)
}

mckappa <- function(dat) {

  agreement <- sum(diag(dat)) / sum(dat)
  expected  <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  (agreement - expected) / (1 - expected)

}

mcserr <- function(dat, kappa) {
  expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  serr(dat, kappa, expected)
}

mcconf <- function(std_err, kappa) {

  alpha    <- 0.05
  interval <- stats::qnorm(1 - (alpha / 2)) * std_err
  ci_lower <- kappa - interval
  ci_upper <- kappa + interval

  list(ci_lower = ci_lower, ci_upper = ci_upper)

}

prop_fact <- function(dat, p) {

  dat_per    <- dat / sum(dat)
  row_sum    <- rowSums(dat_per)
  col_sum    <- colSums(dat_per)
  controls   <- 1 - col_sum[2]
  cases      <- 1 - row_sum[2]
  ratio      <- cases / controls
  odds_ratio <- p[1] / p[2]

  list(cases      = cases,
       controls   = controls,
       odds_ratio = odds_ratio,
       ratio      = ratio

  )

}

serr <- function(dat, kappa, expected) {

  dat_per    <- dat / sum(dat)
  row_sum    <- rowSums(dat_per)
  row_sum[3] <- sum(row_sum)
  col_sum    <- colSums(dat_per)
  dat_per    <- rbind(dat_per, col_sum)
  dat_per    <- cbind(dat_per, row_sum)
  d1         <- dim(dat_per)

  dat_per[d1[1], d1[2]] <- 1.0
  diagonal <- diag(dat_per)

  a <- diagonal[1] * (1 - (row_sum[1] + col_sum[1]) * (1 - kappa)) ^ 2 +
    diagonal[2] * (1 - (row_sum[2] + col_sum[2]) * (1 - kappa)) ^ 2

  x1 <- dat_per[lower.tri(dat_per)][1]
  x2 <- dat_per[upper.tri(dat_per)][1]

  b <- ((1 - kappa) ^ 2) * ((x1 * (row_sum[1] + col_sum[2]) ^ 2) +
    (x2 * (row_sum[2] + col_sum[1]) ^ 2))

  c <- ((kappa) - expected * (1 - kappa)) ^ 2
  variance <- ((a + b - c) / ((1 - expected) ^ 2)) / sum(dat)

  sqrt(variance)
}

mccomp <- function(dat) {

  p         <- mctestp(dat)
  test_stat <- tetat(p)
  df        <- nrow(dat) - 1
  pvalue    <- mcpval(test_stat, df)
  exactp    <- mcpex(dat)
  cstat     <- mcstat(p)
  cpvalue   <- mccpval(cstat, df)
  kappa     <- mckappa(dat)
  std_err   <- mcserr(dat, kappa)
  clu       <- mcconf(std_err, kappa)
  k         <- prop_fact(dat, p)

  list(cases     = round(k$cases, 4),
       controls  = round(k$controls, 4),
       cpvalue   = cpvalue,
       cstat     = cstat,
       df        = df,
       exactp    = round(exactp, 4),
       kappa     = round(kappa, 4),
       kappa_cil = round(clu$ci_lower, 4),
       kappa_ciu = round(clu$ci_upper, 4),
       odratio   = round(k$odds_ratio, 4),
       pvalue    = round(pvalue, 4),
       ratio     = round(k$ratio, 4),
       statistic = round(test_stat, 4),
       std_err   = round(std_err, 4))

}
