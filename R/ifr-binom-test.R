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
#' @return \code{ifr_binom_test} returns an object of class \code{"ifr_binom_test"}.
#' An object of class \code{"ifr_binom_test"} is a list containing the
#' following components:
#'
#' \item{exp_k}{expected number of successes}
#' \item{exp_p}{expected probability of success}
#' \item{k}{number of successes}
#' \item{n}{number of observations}
#' \item{obs_p}{assumed probability of success}
#' \item{pval_lower}{lower one sided p value}
#' \item{pval_upper}{upper one sided p value}
#'
#' @section Deprecated Functions:
#' \code{infer_binom_calc()} and \code{infer_binom_test()} have been deprecated. Instead use
#' \code{ifr_binom_cal()} and \code{ifr_binom_test()}.
#'
#' @references Hoel, P. G. 1984. Introduction to Mathematical Statistics.
#' 5th ed. New York: Wiley.
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
      pval_upper = k$upper
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
  ifr_binom_calc.default(n, k, prob)
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
  p_opp <- round(dbinom(k, n, prob), 9)
  i_p   <- dbinom(exp_k, n, prob)
  i_k   <- exp_k

  if (k < exp_k) {
    while (i_p > p_opp) {
      i_k <- i_k + 1
      i_p <- round(dbinom(i_k, n, prob), 9)
      if (round(i_p) == p_opp) {
        break
      }
    }

    ttf <- pbinom(k, n, prob, lower.tail = T) + pbinom(i_k - 1, n, prob, lower.tail = F)
  } else {
    while (p_opp <= i_p) {
      i_k <- i_k - 1
      i_p <- dbinom(i_k, n, prob)
      if (round(i_p) == p_opp) {
        break
      }
    }

    i_k <- i_k
    tt  <- pbinom(i_k, n, prob, lower.tail = T) + pbinom(k - 1, n, prob, lower.tail = F)
    ttf <- ifelse(tt <= 1, tt, 1)
  }

  list(exp_k    = exp_k,
       exp_p    = prob,
       ik       = i_k,
       k        = k,
       lower    = round(lt, 6),
       n        = n,
       obs_p    = obs_p,
       two_tail = round(ttf, 6),
       upper    = round(ut, 6)
  )

}
