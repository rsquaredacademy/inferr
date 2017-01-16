#' @importFrom stats pbinom dbinom
#' @title Binomial Test
#' @description Test whether the proportion of successes on a two-level
#' categorical dependent variable significantly differs from a hypothesized value.
#' @param n number of observations
#' @param success number of successes
#' @param prob assumed probability of success on a trial
#' @param data a numeric vector
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

        n <- n
        k <- success
    obs_p <- k / n
    exp_k <- round(n * prob)
       lt <- pbinom(k, n, prob, lower.tail = T)
       ut <- pbinom(k - 1, n, prob, lower.tail = F)
    p_opp <- round(dbinom(k, n, prob), 9)
      i_p <- dbinom(exp_k, n, prob)
      i_k <- exp_k

      if (k < exp_k) {

          while (i_p > p_opp) {
              i_k <- i_k + 1
              i_p <- round(dbinom(i_k, n, prob), 9)
          }

          ttf <- pbinom(k, n, prob, lower.tail = T) +
              pbinom(i_k - 1, n, prob, lower.tail = F)

        } else {

          while (p_opp <= i_p) {
              i_k <- i_k - 1
              i_p <- dbinom(i_k, n, prob)
          }

          i_k <- i_k

          tt <- pbinom(i_k, n, prob, lower.tail = T) +
              pbinom(k - 1, n, prob, lower.tail = F)

          ttf <- ifelse(tt <= 1, tt, 1)

          }


    out <- list(
             n = n,
             k = k,
         exp_k = exp_k,
         obs_p = obs_p,
         exp_p = prob,
            ik = i_k,
         lower = round(lt, 6),
         upper = round(ut, 6),
      two_tail = round(ttf, 6)
    )

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
