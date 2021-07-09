#' @useDynLib inferr
#' @importFrom Rcpp sourceCpp
#' @title Test for Random Order
#' @description runtest tests whether the observations of \code{x} are serially
#' independent i.e. whether they occur in a random order, by counting
#' how many runs there are above and below a threshold.  By default, the median
#' is used as the threshold.  A small number of runs indicates positive serial
#' correlation; a large number indicates negative serial correlation.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param drop logical; if TRUE, values equal to the threshold will be dropped
#' from \code{x}
#' @param split logical; if TRUE, data will be recoded in binary format
#' @param mean logical; if TRUE, mean will be used as threshold
#' @param threshold threshold to be used for counting runs, specify 0 if data
#' is coded as a binary.
#' @return \code{ifr_runs_test} returns an object of class \code{"ifr_runs_test"}.
#' An object of class \code{"ifr_runs_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{threshold}{within group sum of squares}
#' \item{n_below}{number below the threshold}
#' \item{n_above}{number above the threshold}
#' \item{mean}{expected number of runs}
#' \item{var}{variance of the number of runs}
#' \item{n_runs}{number of runs}
#' \item{z}{z statistic}
#' \item{p}{p-value of \code{z}}
#' @section Deprecated Function:
#' \code{runs_test()} has been deprecated. Instead use \code{ifr_runs_test()}.
#' @references
#' {Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric Statistical Procedures, 4th edition. : Chapman & Hall/CRC.}
#'
#' {Edgington, E. S. 1961. Probability table for number of runs of signs of first differences in ordered series. Journal of the American Statistical Association 56: 156–159.}
#'
#' {Madansky, A. 1988. Prescriptions for Working Statisticians. New York: Springer.}
#'
#' {Swed, F. S., and C. Eisenhart. 1943. Tables for testing randomness of grouping in a sequence of alternatives. Annals of Mathematical Statistics 14: 66–87.}
#' @examples
#' ifr_runs_test(hsb, read)
#'
#' ifr_runs_test(hsb, read, drop = TRUE)
#'
#' ifr_runs_test(hsb, read, split = TRUE)
#'
#' ifr_runs_test(hsb, read, mean = TRUE)
#'
#' ifr_runs_test(hsb, read, threshold = 0)
#'
#' @importFrom stats pnorm
#'
#' @export
#'
ifr_runs_test <- function(data, x, drop = FALSE, split = FALSE, mean = FALSE,
                            threshold = NA) UseMethod("ifr_runs_test")

#' @export
#'
ifr_runs_test.default <- function(data, x, drop = FALSE,
                                    split = FALSE, mean = FALSE,
                                    threshold = NA) {

  x1   <- deparse(substitute(x))
  xone <- data[[x1]]
  n    <- length(xone)

  if (is.na(threshold)) {
    y <- unique(xone)
    if (sum(y) == 1) {
      stop("Use 0 as threshold if the data is coded as a binary.", call. = FALSE)
    }
  }

  if (!(is.na(threshold))) {
    thresh <- threshold
  } else if (mean) {
    thresh <- mean(xone)
  } else {
    thresh <- median(xone, na.rm = TRUE)
  }

  if (drop) {
    xone <- xone[xone != thresh]
  }

  if (split) {
    x_binary <- ifelse(xone > thresh, 1, 0)
  } else {

    x_binary <-
      xone %>%
      lapply(nruns2, thresh) %>%
      unlist(use.names = FALSE)
  }

  n_runs   <- nsignC(x_binary)
  n1       <- sum(x_binary)
  n0       <- length(x_binary) - n1
  exp_runs <- expruns(n0, n1)
  sd_runs  <- sdruns(n0, n1)

  test_stat <- (n_runs - exp_runs) / (sd_runs ^ 0.5)
  sig <- 2 * (1 - pnorm(abs(test_stat), lower.tail = TRUE))

  result <-
    list(mean      = exp_runs,
         n         = n,
         n_above   = n1,
         n_below   = n0,
         n_runs    = n_runs,
         p         = sig,
         threshold = thresh,
         var       = sd_runs,
         z         = test_stat)

  class(result) <- "ifr_runs_test"
  return(result)
}

#' @export
#'
print.ifr_runs_test <- function(x, ...) {
  print_runs_test(x)
}

# expected runs
expruns <- function(n0, n1) {
  N <- n0 + n1
  return(((2 * n0 * n1) / N) + 1)
}

# standard deviation of runs
sdruns <- function(n0, n1) {
  N <- n0 + n1
  n <- 2 * n0 * n1
  return(((n * (n - N)) / ((N ^ 2) * (N - 1))))
}

nruns2 <- function(data, value) {
  if (data <= value) {
    return(0)
  } else {
    return(1)
  }
}
