#' @title Binomial Test
#' @description Test whether the proportion of successes on a two-level
#' categorical dependent variable significantly differs from a hypothesized value.
#' @param n number of observations
#' @param success number of successes
#' @param prob assumed probability of success on a trial
#' @param data a \code{data.frame} or a \code{tibble}
#' @param variable factor; column in \code{data}
#' @param ... additional arguments passed to or from other methods
#'
#' @details The success is the \strong{second} level of the factor. For a
#' character column the levels are ordered alphabetically, as \code{factor()}
#' does, so for a binary column the second (last) level is the success. For a
#' logical column the levels are \code{FALSE} and \code{TRUE}, so \code{TRUE}
#' is the success; for a numeric 0/1 column the levels are \code{0} and
#' \code{1}, so \code{1} is the success. \code{ifr_binom_test()} requires the
#' column to be a factor, so convert character, logical or 0/1 columns with
#' \code{factor()} first and check the level order with \code{levels()}.
#'
#' @return \code{ifr_binom_test} returns an object of class
#' \code{c("ifr_binom_test", "ifr_binom_calc")}. An object of this class is a
#' list containing the following components:
#'
#' \item{exp_k}{expected number of successes}
#' \item{exp_p}{expected probability of success}
#' \item{k}{number of successes}
#' \item{n}{number of observations}
#' \item{obs_p}{assumed probability of success}
#' \item{pval_lower}{lower one sided p value}
#' \item{pval_upper}{upper one sided p value}
#' \item{two_tail}{two sided exact p value}
#'
#' @section Deprecated Functions:
#' \code{infer_binom_calc()} and \code{infer_binom_test()} have been deprecated. Instead use
#' \code{ifr_binom_cal()} and \code{ifr_binom_test()}.
#'
#' @references Hoel, P. G. 1984. Introduction to Mathematical Statistics.
#' 5th ed. New York: Wiley.
#'
#' Conover, W. J. (1999). \emph{Practical Nonparametric Statistics}, 3rd ed. New York: Wiley.
#'
#' Hollander, M., Wolfe, D. A. and Chicken, E. (2014). \emph{Nonparametric Statistical Methods}, 3rd ed. Hoboken, NJ: Wiley.
#'
#' @examples
#' # using calculator
#' ifr_binom_calc(32, 13, prob = 0.5)
#'
#' # using data set
#' ifr_binom_test(hsb, female, prob = 0.5)
#'
#' @seealso \code{\link[stats]{binom.test}}
#'
#' @export
#'
ifr_binom_calc <- function(n, success, prob = 0.5, ...) UseMethod("ifr_binom_calc")

#' @export
ifr_binom_calc.default <- function(n, success, prob = 0.5, ...) {

  if (!is.numeric(n)) {
    stop("n must be an integer", call. = FALSE)
  }

  if (!is.numeric(success)) {
    stop("success must be an integer", call. = FALSE)
  }

  if (!is.numeric(prob)) {
    stop("prob must be numeric", call. = FALSE)
  }

  if ((prob < 0) | (prob > 1)) {
    stop("prob must be between 0 and 1", call. = FALSE)
  }

  k <- binom_comp(n, success, prob)

  out <-
    list(
      exp_k      = k$exp_k,
      exp_p      = k$exp_p,
      k          = k$k,
      n          = n,
      obs_p      = k$obs_p,
      pval_lower = k$lower,
      pval_upper = k$upper,
      two_tail   = k$two_tail
  )

  class(out) <- "ifr_binom_calc"
  return(out)
}

#' @export
#' @rdname ifr_binom_calc
#' @usage NULL
#'
infer_binom_calc <- function(n, success, prob = 0.5, ...) {
  .Deprecated("ifr_binom_calc()")
}

#' @export
print.ifr_binom_calc <- function(x, ...) {
  print_binom(x)
}

#' @export
#' @rdname ifr_binom_calc
ifr_binom_test <- function(data, variable, prob = 0.5) {

  varyable <- deparse(substitute(variable))
  fdata    <- data[[varyable]]

  if (!is.factor(fdata)) {
    stop("variable must be of type factor", call. = FALSE)
  }

  if (nlevels(fdata) > 2) {
    stop("Binomial test is applicable only to binary data i.e. categorical data with 2 levels.", call. = FALSE)
  }

  if (!is.numeric(prob)) {
    stop("prob must be numeric", call. = FALSE)
  }

  if ((prob < 0) | (prob > 1)) {
    stop("prob must be between 0 and 1", call. = FALSE)
  }

  n <- length(fdata)
  k <- table(fdata)[[2]]

  out <- ifr_binom_calc.default(n, k, prob)
  class(out) <- c("ifr_binom_test", "ifr_binom_calc")
  out
}

#' @export
#' @rdname ifr_binom_calc
#' @usage NULL
#'
infer_binom_test <- function(data, variable, prob = 0.5) {
  .Deprecated("ifr_binom_test()")
}

#' @importFrom stats pbinom dbinom
binom_comp <- function(n, success, prob) {

  n     <- n
  k     <- success
  obs_p <- k / n
  exp_k <- round(n * prob)
  lt    <- pbinom(k, n, prob, lower.tail = T)
  ut    <- pbinom(k - 1, n, prob, lower.tail = F)

  # two-sided p-value using the rule of stats::binom.test(): sum the
  # probabilities of all outcomes no more likely than the observed one
  if (prob == 0) {
    ttf <- as.numeric(k == 0)
  } else if (prob == 1) {
    ttf <- as.numeric(k == n)
  } else {
    rel_err <- 1 + 1e-07
    d       <- dbinom(k, n, prob)
    m       <- n * prob

    if (k == m) {
      ttf <- 1
    } else if (k < m) {
      i   <- seq.int(from = ceiling(m), to = n)
      y   <- sum(dbinom(i, n, prob) <= d * rel_err)
      ttf <- pbinom(k, n, prob) +
             pbinom(n - y, n, prob, lower.tail = FALSE)
    } else {
      i   <- seq.int(from = 0, to = floor(m))
      y   <- sum(dbinom(i, n, prob) <= d * rel_err)
      ttf <- pbinom(y - 1, n, prob) +
             pbinom(k - 1, n, prob, lower.tail = FALSE)
    }
  }

  list(exp_k    = exp_k,
       exp_p    = prob,
       k        = k,
       lower    = round(lt, 6),
       n        = n,
       obs_p    = obs_p,
       two_tail = ttf,
       upper    = round(ut, 6)
  )

}
