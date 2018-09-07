#' @importFrom rlang quos !!!
#' @title Cochran Q Test
#' @description Test if the proportions of 3 or more dichotomous variables are
#' equal in the same population.
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... columns in \code{data}
#' @return \code{infer_cochran_qtest} returns an object of class
#' \code{"infer_cochran_qtest"}. An object of class \code{"infer_cochran_qtest"}
#' is a list containing the following components:
#'
#' \item{n}{number of observations}
#' \item{df}{degrees of freedom}
#' \item{q}{cochran's q statistic}
#' \item{pvalue}{p-value}
#' @section Deprecated Function:
#' \code{cochran_test()} has been deprecated. Instead use
#' \code{infer_cochran_qtest()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' infer_cochran_qtest(exam, exam1, exam2, exam3)
#' @export
#'
infer_cochran_qtest <- function(data, ...) UseMethod("infer_cochran_qtest")

#' @export
infer_cochran_qtest.default <- function(data, ...) {
  
  vars <- quos(...)

  fdata <- select(data, !!! vars)

  if (ncol(fdata) < 3) {
    stop("Please specify at least 3 variables.")
  }

  if (any(sapply(lapply(fdata, as.factor), nlevels) > 2)) {
    stop("Please specify dichotomous/binary variables only.")
  }

  k <- cochran_comp(fdata)
  result <- list(n = k$n, df = k$df, q = k$q, pvalue = k$pvalue)
  class(result) <- "infer_cochran_qtest"
  return(result)
}

#' @export
#' @rdname infer_cochran_qtest
#' @usage NULL
#'
cochran_test <- function(x, ...) {
  .Deprecated("infer_cochran_qtest()")
}

#' @export
#'
print.infer_cochran_qtest <- function(x, ...) {
  print_cochran_test(x)
}
