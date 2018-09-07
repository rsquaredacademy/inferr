#' @title Chi Square Goodness of Fit Test
#' @description Test whether the observed proportions for a categorical variable
#' differ from hypothesized proportions
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y expected proportions
#' @param correct logical; if TRUE continuity correction is applied
#' @return \code{infer_chisq_gof_test} returns an object of class
#' \code{"infer_chisq_gof_test"}. An object of class \code{"infer_chisq_gof_test"}
#' is a list containing the following components:
#'
#' \item{chisquare}{chi square statistic}
#' \item{pvalue}{p-value}
#' \item{df}{chi square degrees of freedom}
#' \item{ssize}{number of observations}
#' \item{names}{levels of \code{x}}
#' \item{level}{number of levels of \code{x}}
#' \item{obs}{observed frequency/proportion}
#' \item{exp}{expected frequency/proportion}
#' \item{deviation}{deviation of observed from frequency}
#' \item{std}{standardized residuals}
#' \item{varname}{name of categorical variable}
#' @section Deprecated Function:
#' \code{chisq_gof()} has been deprecated. Instead use
#' \code{infer_chisq_gof_test()}
#'
#' @seealso \code{\link[stats]{chisq.test}}
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @examples
#' infer_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
#'
#' # apply continuity correction
#' infer_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
#' @export
#'
infer_chisq_gof_test <- function(data, x, y, correct = FALSE) UseMethod("infer_chisq_gof_test")

#' @export
infer_chisq_gof_test.default <- function(data, x, y, correct = FALSE) {

  x1 <- enquo(x)
  xcheck <- pull(data, !! x1)

  xlen <-
    data %>%
    pull(!! x1) %>%
    length()

  xone <-
    data %>%
    pull(!! x1) %>%
    table() %>%
    as.vector()

  if (!is.factor(xcheck)) {
    stop("x must be an object of class factor")
  }

  if (!is.numeric(y)) {
    stop("y must be numeric")
  }

  if (!is.logical(correct)) {
    stop("correct must be either TRUE or FALSE")
  }


  varname <-
    data %>%
    select(!! x1) %>%
    names()

  n <- length(xone)

  if (length(y) != n) {
    stop("Length of y must be equal to the number of categories in x")
  }

  df <- n - 1

  if (sum(y) == 1) {
    y <- xlen * y
  }

  if ((df == 1) || (correct == TRUE)) {
    k <- chi_cort(xone, y)
  } else {
    k <- chigof(xone, y)
  }

  sig <- round(pchisq(k$chi, df, lower.tail = FALSE), 4)

  result <- list(
    chisquare = k$chi, pvalue = sig, df = df, ssize = length(xcheck),
    names = levels(xcheck), level = nlevels(xcheck), obs = xone, exp = y,
    deviation = format(k$dev, nsmall = 2), std = format(k$std, nsmall = 2),
    varname = varname
  )

  class(result) <- "infer_chisq_gof_test"
  return(result)
}

#' @export
#' @rdname infer_chisq_gof_test
#' @usage NULL
#'
chisq_gof <- function(x, y, correct = FALSE) {
  .Deprecated("infer_chisq_gof_test()")
}

#' @export
print.infer_chisq_gof_test <- function(x, ...) {
  print_chisq_gof(x)
}
