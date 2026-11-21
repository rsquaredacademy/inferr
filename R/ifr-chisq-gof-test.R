#' @title Chi Square Goodness of Fit Test
#' @description Test whether the observed proportions for a categorical variable
#' differ from hypothesized proportions
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y finite, non-negative expected counts or proportions, in the order
#' of the declared non-missing levels of \code{x}; see Details for the
#' count/proportion rule
#' @param correct logical; if TRUE continuity correction is applied
#' @details Supported inputs: at least two declared non-missing categories,
#' finite non-negative expected values with absolute value \code{<= 1e150},
#' and sample sizes \code{<= 1e15}. Missing observations in \code{x} (including
#' values stored in an explicit \code{NA} factor level) are removed with a
#' message. The explicit \code{NA} level is dropped, but every other declared
#' level is retained even when it is unobserved, because it is part of the
#' hypothesis.
#'
#' Expected values in \code{y} must have one entry per retained level. They are
#' treated as proportions when they sum to one within a small tolerance, or
#' when every value is one (the equal-proportions shorthand). Otherwise they
#' are counts and must sum to the sample size. The tolerance for a proportion
#' total is relative to that total, not to the sample size, so a vector such as
#' \code{c(.5, .501)} is rejected instead of being accepted as proportions. A
#' zero expected count is always rejected with an error identifying its
#' position and category, because the statistic and its standardized residuals
#' are undefined for that cell. A factor with only one retained category is
#' rejected because a goodness-of-fit test with zero degrees of freedom does
#' not exist.
#'
#' The chi-square statistic is stored to four decimal places. The p-value is
#' stored unrounded in \code{pvalue_raw} and to four decimal places in
#' \code{pvalue}; it is printed as a bound when it is below the display
#' resolution.
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
#' \item{pvalue}{p-value, stored to four decimal places}
#' \item{pvalue_raw}{unrounded p-value}
#' \item{sample_size}{number of observations}
#' \item{std_residuals}{standardized residuals}
#' \item{varname}{name of categorical variable}
#'
#' @section Deprecated Function:
#' \code{infer_chisq_gof_test()} has been deprecated. Instead use
#' \code{ifr_chisq_gof_test()}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' Yates, F. (1934). Contingency tables involving small numbers and the chi-squared test. \emph{Supplement to the Journal of the Royal Statistical Society}, 1(2), 217--235.
#'
#' @examples
#' ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140))
#'
#' # apply continuity correction
#' ifr_chisq_gof_test(hsb, race, c(20, 20, 20, 140), correct = TRUE)
#'
#' @seealso \code{\link[stats]{chisq.test}}
#'
#' @export
#'
ifr_chisq_gof_test <- function(data, x, y, correct = FALSE) UseMethod("ifr_chisq_gof_test")

#' @export
ifr_chisq_gof_test.default <- function(data, x, y, correct = FALSE) {

  x1     <- deparse(substitute(x))
  xcheck <- data[[x1]]

  if (!is.factor(xcheck)) {
    stop("x must be an object of class factor", call. = FALSE)
  }

  # an explicit NA factor level is stored as a level code, so is.na() does not
  # see it; detect that level as well as ordinary missing values
  na_levels <- which(is.na(levels(xcheck)))
  missing_x <- is.na(xcheck)
  if (length(na_levels) > 0L) {
    missing_x <- missing_x | as.integer(xcheck) %in% na_levels
  }

  if (any(missing_x)) {
    n_missing <- sum(missing_x)
    message("removing ", n_missing, " observation(s) with missing '", x1, "'")
    xcheck <- xcheck[!missing_x]
  }

  # drop only the explicit NA level; declared but unobserved non-NA levels are
  # part of the hypothesis and must be retained
  if (length(na_levels) > 0L) {
    keep_levels <- levels(xcheck)[!is.na(levels(xcheck))]
    xcheck      <- factor(as.character(xcheck), levels = keep_levels)
  }

  if (length(xcheck) < 1L) {
    stop("x must contain at least one non-missing observation", call. = FALSE)
  }

  xone   <- as.vector(table(xcheck))
  xlen   <- sum(xone)
  n      <- length(xone)

  if (n < 2L) {
    stop(paste0("the chi-square goodness-of-fit test requires at least two ",
                "categories with positive expected values"),
         call. = FALSE)
  }

  check_max_sample_size(xlen, "sample size")

  if (!is.numeric(y)) {
    stop("y must be numeric", call. = FALSE)
  }

  if (any(!is.finite(y)) || any(y < 0)) {
    stop("y must contain non-negative expected counts or proportions",
         call. = FALSE)
  }

  if (any(y > 1e150)) {
    stop("expected values in y must have absolute value <= 1e150",
         call. = FALSE)
  }

  if (sum(y) <= 0) {
    stop("y must contain at least one positive expected count",
         call. = FALSE)
  }

  if (!is.logical(correct)) {
    stop("correct must be either TRUE or FALSE", call. = FALSE)
  }

  varname <- names(data[x1])
  df      <- n - 1

  if (length(y) != n) {
    stop(paste0("Length of y must be equal to the number of categories in x ",
                "after removing missing observations"),
         call. = FALSE)
  }

  # expected values are counts unless they sum to one, in which case they are
  # proportions; a vector of ones is the equal-proportions shorthand. A count
  # vector must total the observed sample size. Proportions and counts use
  # separate tolerances so a proportion total is never compared on the scale of
  # the sample size.
  y_sum <- sum(y)

  if (!is.finite(y_sum)) {
    stop("the sum of expected values in y must be finite", call. = FALSE)
  }

  tol_prop  <- sqrt(.Machine$double.eps) * max(1, abs(y_sum))
  tol_count <- sqrt(.Machine$double.eps) * max(1, abs(xlen), abs(y_sum))

  if (abs(y_sum - 1) <= tol_prop || all(y == 1)) {
    y <- xlen * (y / y_sum)
  } else if (abs(y_sum - xlen) > tol_count) {
    stop(paste0("expected counts in y must sum to the sample size (",
                xlen, "); expected values in y are counts unless they sum to 1"),
         call. = FALSE)
  }

  zero_expected <- which(y == 0)
  if (length(zero_expected) > 0) {
    cell <- zero_expected[1]
    stop(paste0("y[", cell, "] for category '", levels(xcheck)[cell],
                "' of '", x1,
                "' is zero; the chi-square goodness-of-fit test is undefined"),
         call. = FALSE)
  }

  if (correct == TRUE) {
    k <- chi_cort(xone, y)
  } else {
    k <- chigof(xone, y)
  }

  sig <- pchisq(k$raw, df, lower.tail = FALSE)

  result <-
    list(
      categories         = levels(xcheck),
      chisquare          = k$chi,
      deviation          = format(k$dev, nsmall = 2),
      degrees_of_freedom = df,
      expected_frequency = y,
      n_levels           = nlevels(xcheck),
      observed_frequency = xone,
      pvalue             = round(sig, 4),
      pvalue_raw         = sig,
      sample_size        = xlen,
      std_residuals      = format(k$std, nsmall = 2),
      varname            = varname
  )

  class(result) <- "ifr_chisq_gof_test"
  return(result)
}

#' @export
#' @rdname ifr_chisq_gof_test
#' @usage NULL
#'
infer_chisq_gof_test <- function(data, x, y, correct = FALSE) {
  .Deprecated("ifr_chisq_gof_test()")
}

#' @export
print.ifr_chisq_gof_test <- function(x, ...) {
  print_chisq_gof(x)
}

chi_cort <- function(x, y) {

  dif  <- pmax(abs(x - y) - 0.5, 0)
  diff <- sign(x - y) * dif
  dif2 <- dif ^ 2
  dev  <- round((diff / y) * 100, 2)
  std  <- round(diff / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev,
       std = std,
       chi = chi,
       raw = sum(dif2 / y))
}

chigof <- function(x, y) {

  dif  <- x - y
  dif2 <- dif ^ 2
  dev  <- round((dif / y) * 100, 2)
  std  <- round(dif / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev,
       std = std,
       chi = chi,
       raw = sum(dif2 / y))
}
