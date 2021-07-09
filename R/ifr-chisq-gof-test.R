#' @title Chi Square Goodness of Fit Test
#' @description Test whether the observed proportions for a categorical variable
#' differ from hypothesized proportions
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y expected proportions
#' @param correct logical; if TRUE continuity correction is applied
#' @return \code{ifr_chisq_gof_test} returns an object of class
#' \code{"ifr_chisq_gof_test"}. An object of class \code{"ifr_chisq_gof_test"}
#' is a list containing the following components:
#'
#' \item{categories}{levels of \code{x}}
#' \item{chisquare}{chi square statistic}
#' \item{deviation}{deviation of observed from frequency}
#' \item{degrees_of_freedom}{chi square degrees of freedom}
#' \item{expected_frequency}{expected frequency/proportion}
#' \item{n_levels}{number of levels of \code{x}}
#' \item{observed_frequency}{observed frequency/proportion}
#' \item{pvalue}{p-value}
#' \item{sample_size}{number of observations}
#' \item{std_residuals}{standardized residuals}
#' \item{varname}{name of categorical variable}
#'
#' @section Deprecated Function:
#' \code{chisq_gof()} has been deprecated. Instead use
#' \code{ifr_chisq_gof_test()}
#'
#' @seealso \code{\link[stats]{chisq.test}}
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @examples
#' ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
#'
#' # apply continuity correction
#' ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
#' @export
#'
ifr_chisq_gof_test <- function(data, x, y, correct = FALSE) UseMethod("ifr_chisq_gof_test")

#' @export
ifr_chisq_gof_test.default <- function(data, x, y, correct = FALSE) {

  x1     <- deparse(substitute(x))
  xcheck <- data[[x1]]
  xlen   <- length(data[[x1]])
  xone   <- as.vector(table(data[[x1]]))

  if (!is.factor(xcheck)) {
    stop("x must be an object of class factor", call. = FALSE)
  }

  if (!is.numeric(y)) {
    stop("y must be numeric", call. = FALSE)
  }

  if (!is.logical(correct)) {
    stop("correct must be either TRUE or FALSE", call. = FALSE)
  }

  varname <- names(data[x1])
  n       <- length(xone)
  df      <- n - 1

  if (length(y) != n) {
    stop("Length of y must be equal to the number of categories in x", call. = FALSE)
  }

  if (sum(y) == 1) {
    y <- xlen * y
  }

  if ((df == 1) || (correct == TRUE)) {
    k <- chi_cort(xone, y)
  } else {
    k <- chigof(xone, y)
  }

  sig <- round(pchisq(k$chi, df, lower.tail = FALSE), 4)

  result <-
    list(
      categories         = levels(xcheck),
      chisquare          = k$chi,
      deviation          = format(k$dev, nsmall = 2),
      degrees_of_freedom = df,
      expected_frequency = y,
      n_levels           = nlevels(xcheck),
      observed_frequency = xone,
      pvalue             = sig,
      sample_size        = length(xcheck),
      std_residuals      = format(k$std, nsmall = 2),
      varname            = varname
  )

  class(result) <- "ifr_chisq_gof_test"
  return(result)
}

#' @export
print.ifr_chisq_gof_test <- function(x, ...) {
  print_chisq_gof(x)
}

chi_cort <- function(x, y) {

  diff <- x - y - 0.5
  dif  <- abs(x - y) - 0.5
  dif2 <- dif ^ 2
  dev  <- round((diff / y) * 100, 2)
  std  <- round(diff / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev, 
       std = std, 
       chi = chi)
}

chigof <- function(x, y) {

  dif  <- x - y
  dif2 <- dif ^ 2
  dev  <- round((dif / y) * 100, 2)
  std  <- round(dif / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev, 
       std = std, 
       chi = chi)
}
