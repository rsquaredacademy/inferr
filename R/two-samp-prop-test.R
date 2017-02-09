#' @title Two Sample Test of Proportion
#' @description Tests on the equality of proportions using
#' large-sample statistics. It tests that \code{var} has the same proportion
#' within two groups defined by \code{group} or that two variables \code{var1} and
#' \code{var2} have the same proportion.
#' @param var1 a categorical variable
#' @param var2 a categorical variable
#' @param var a categorical variable
#' @param group a categorical variable
#' @param n1 sample 1 size
#' @param n2 sample 2 size
#' @param p1 sample 1 proportion
#' @param p2 sample 2 proportion
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return an object of class \code{"prop_test"}.
#' An object of class \code{"prop_test"} is a list containing the
#' following components:
#'
#' \item{n1}{sample 1 size}
#' \item{n2}{sample 2 size}
#' \item{phat1}{sample 1 proportion}
#' \item{phat2}{sample 2 proportion}
#' \item{z}{z statistic}
#' \item{sig}{p-value for z statistic}
#' \item{alt}{alternative hypothesis}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{prop.test}}
#' @examples
#' # using variables
#' # lower tail
#' ts_prop_test(var1 = treatment$treatment1, var2 = treatment$treatment2, alternative = 'less')
#'
#' # upper tail
#' ts_prop_test(var1 = treatment$treatment1, var2 = treatment$treatment2, alternative = 'greater')
#'
#' # both tails
#' ts_prop_test(var1 = treatment$treatment1, var2 = treatment$treatment2, alternative = 'both')
#'
#' # all tails
#' ts_prop_test(var1 = treatment$treatment1, var2 = treatment$treatment2, alternative = 'all')
#'
#' # using groups
#' # lower tail
#' ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'less')
#'
#' # upper tail
#' ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'greater')
#'
#' # both tails
#' ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'both')
#'
#' # # all tails
#' ts_prop_grp(var = treatment2$outcome, group = treatment2$female, alternative = 'all')
#'
#' # using sample size and proportions
#' # lower tail
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'less')
#'
#' # upper tail
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'greater')
#'
#' # both tails
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'both')
#'
#' # all tails
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'all')
#' @export
#'
ts_prop_test <- function(var1, var2,
  alternative = c('both', 'less', 'greater', 'all'), ...) UseMethod('ts_prop_test')

#' @export
#'
ts_prop_test.default <- function(var1, var2,
  alternative = c('both', 'less', 'greater', 'all'), ...) {

  alt <- match.arg(alternative)
    k <- prop_comp2(var1, var2, alt)

  result <- list(n1 = k$n1, n2 = k$n2, phat1 = k$phat1, phat2 = k$phat2, 
    z = k$z, sig = k$sig, alt = alt)

  class(result) <- 'ts_prop_test'
  return(result)

}

#' @export
#'
print.ts_prop_test <- function(x, ...) {
  print_ts_prop_test(x)
}


#' @export
#' @rdname ts_prop_test
#'
ts_prop_grp <- function(var, group,
  alternative = c('both', 'less', 'greater', 'all')) {


	    n <- tapply(var, group, length)
	   n1 <- n[[1]]
	   n2 <- n[[2]]
	    y <- tapply(var, group, table)
	   y1 <- y[[1]][[2]]
	   y2 <- y[[2]][[2]]
	phat1 <- y1 / n1
	phat2 <- y2 / n2
	 phat <- sum(y1, y2) / sum(n1, n2)
	  num <- (phat1 - phat2)
	 den1 <- phat * (1 - phat)
	 den2 <- (1 / n1) + (1 / n2)
	  den <- sqrt(den1 * den2)
	    z <- num / den


	lt <- pnorm(z)
	ut <- round(pnorm(z, lower.tail = FALSE), 4)
	tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('both' = tt, 'less' = lt, 'greater' = ut)
    } else if (alt == "greater") {
        sig = ut
    } else if (alt == "less"){
        sig = lt
    } else {
        sig = tt
    }

    out <- list(
         n1 = n1,
         n2 = n2,
      phat1 = phat1,
      phat2 = phat2,
          z = round(z, 3),
        sig = round(sig, 3),
        alt = alt)

    class(out) <- 'ts_prop_test'
    return(out)

}


#' @export
#' @rdname ts_prop_test
#'
ts_prop_calc <- function(n1, n2, p1, p2,
  alternative = c('both', 'less', 'greater', 'all'), ...) {

	   n1 <- n1
	   n2 <- n2
	phat1 <- p1
	phat2 <- p2
	 phat <- sum(n1 * p1, n2 * p2) / sum(n1, n2)
	  num <- (phat1 - phat2)
	 den1 <- phat * (1 - phat)
	 den2 <- (1 / n1) + (1 / n2)
	  den <- sqrt(den1 * den2)
	    z <- num / den

	lt <- pnorm(z)
	ut <- round(pnorm(z, lower.tail = FALSE), 4)
	tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('both' = tt, 'less' = lt, 'greater' = ut)
    } else if (alt == "greater") {
        sig = ut
    } else if (alt == "less"){
        sig = lt
    } else {
        sig = tt
    }

    out <- list(
         n1 = n1,
         n2 = n2,
      phat1 = round(phat1, 3),
      phat2 = round(phat2, 3),
          z = round(z, 3),
        sig = round(sig, 3),
        alt = alt)

    class(out) <- 'ts_prop_test'
    return(out)

}
