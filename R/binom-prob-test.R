#' @importFrom stats pbinom dbinom
#' @title Binomial Test
#' @description Test whether the proportion of successes on a two-level
#' categorical dependent variable significantly differs from a hypothesized value.
#' @param n number of observations
#' @param success number of successes
#' @param prob assumed probability of success on a trial
#' @param data binary/dichotomous factor 
#' @param ... additional arguments passed to or from other methods
#' @return \code{binom_test} returns an object of class \code{"binom_test"}.
#' An object of class \code{"binom_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{k}{number of successes}
#' \item{exp_k}{expected number of successes}
#' \item{obs_p}{assumed probability of success}
#' \item{exp_p}{expected probability of success}
#' \item{ik}{the largest number <= \code{exp_k} such that Pr(k = ik) <= Pr(k = kobs)}
#' \item{lower}{lower one sided p value}
#' \item{upper}{upper one sided p value}
#' \item{two_tail}{two sided p value}
#' @references Hoel, P. G. 1984. Introduction to Mathematical Statistics.
#' 5th ed. New York: Wiley.
#'
#' @seealso \code{\link[stats]{binom.test}}
#' @examples
#' # using calculator
#' binom_calc(32, 13, prob = 0.5)
#'
#' # using data set
#' binom_test(as.factor(hsb$female), prob = 0.5)
#' @export
#'
binom_calc <- function(n, success, prob = 0.5, ...) UseMethod('binom_calc')

#' @export
binom_calc.default <- function(n, success, prob = 0.5, ...) {

    if (!is.numeric(n)) {
      stop('n must be an integer')
    }

    if (!is.numeric(success)) {
      stop('success must be an integer')
    }

    if(!is.numeric(prob)) {
      stop('prob must be numeric')
    }

    if((prob < 0) | (prob > 1)) {
      stop('prob must be between 0 and 1')
    }

    k <- binom_comp(n, success, prob)

    out <- list(n = n, k = k$k, exp_k = k$exp_k, obs_p = k$obs_p, 
           exp_p = k$exp_p, ik = k$ik, lower = k$lower, upper = k$upper, 
           two_tail = k$two_tail)

    class(out) <- 'binom_calc'
    return(out)
}

#' @export
print.binom_calc <- function(x, ...) {
  print_binom(x)
}

#' @export
#' @rdname binom_calc
binom_test <- function(data, prob = 0.5) {

    if (!is.factor(data)) {
      stop('data must be of type factor')
    }

    if (nlevels(data) > 2) {
      stop('Binomial test is applicable only to binary data i.e. categorical data with 2 levels.')
    }

    if(!is.numeric(prob)) {
      stop('prob must be numeric')
    }

    if((prob < 0) | (prob > 1)) {
      stop('prob must be between 0 and 1')
    }

    n <- length(data)
    k <- table(data)[[2]]
    binom_calc.default(n, k, prob)
}
