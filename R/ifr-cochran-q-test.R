#' @title Cochran Q Test
#' @description Test if the proportions of 3 or more dichotomous variables are
#' equal in the same population.
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... columns in \code{data}
#'
#' @return \code{ifr_cochran_qtest} returns an object of class
#' \code{"ifr_cochran_qtest"}. An object of class \code{"ifr_cochran_qtest"}
#' is a list containing the following components:
#'
#' \item{df}{degrees of freedom}
#' \item{n}{number of observations}
#' \item{pvalue}{p value}
#' \item{q}{cochran's q statistic}
#'
#' @section Deprecated Function:
#' \code{infer_cochran_test()} has been deprecated. Instead use
#' \code{ifr_cochran_qtest()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' ifr_cochran_qtest(exam, exam1, exam2, exam3)
#'
#' @export
#'
ifr_cochran_qtest <- function(data, ...) UseMethod("ifr_cochran_qtest")

#' @export
ifr_cochran_qtest.default <- function(data, ...) {

  vars  <- vapply(substitute(...()), deparse, NA_character_)
  fdata <- data[vars]

  if (ncol(fdata) < 3) {
    stop("Please specify at least 3 variables.", call. = FALSE)
  }

  if (any(sapply(lapply(fdata, as.factor), nlevels) > 2)) {
    stop("Please specify dichotomous/binary variables only.", call. = FALSE)
  }

  k <- cochran_comp(fdata)

  result <-
    list(
      df     = k$df,
      n      = k$n,
      pvalue = k$pvalue,
      q      = k$q)

  class(result) <- "ifr_cochran_qtest"
  return(result)
}

#' @export
#' @rdname ifr_cochran_qtest
#' @usage NULL
#'
infer_cochran_qtest <- function(data, ...) {
  .Deprecated("ifr_cochran_qtest()")
}

#' @export
#'
print.ifr_cochran_qtest <- function(x, ...) {
  print_cochran_test(x)
}

coch_data <- function(x, ...) {

  if (is.data.frame(x)) {
    data <- x %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      `-`(1)
  } else {
    data <- cbind(x, ...) %>%
      apply(2, as.numeric) %>%
      `-`(1) %>%
      as.data.frame()
  }

  return(data)
}

cochran_comp <- function(data) {

  n  <- nrow(data)
  k  <- ncol(data)
  df <- k - 1

  cs <-
    data %>%
    lapply(as.numeric) %>%
    as.data.frame() %>%
    subtract(1) %>%
    sums()

  q <- coch(k, cs$cls_sum, cs$cl, cs$g, cs$gs_sum)

  pvalue <- 1 - pchisq(q, df)

  list(
    df     = df,
    n      = n,
    pvalue = round(pvalue, 4),
    q      = q)

}

sums <- function(data) {

  cl      <- colSums(data)
  cls_sum <- sum(cl ^ 2)
  g       <- rowSums(data)
  gs_sum  <- sum(g ^ 2)

  list(
    cl      = cl,
    cls_sum = cls_sum,
    g       = g,
    gs_sum  = gs_sum)

}

coch <- function(k, cls_sum, cl, g, gs_sum) {
  ((k - 1) * ((k * cls_sum) - (sum(cl) ^ 2))) / ((k * sum(g)) - gs_sum)
}

