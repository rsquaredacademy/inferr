#' @title Two sample test of proportion
#' @description tests on the equality of proportions using
#' large-sample statistics.
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
#' just the initial letter.
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
#' @examples
#' # using variables
#' ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'less')
#' ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'greater')
#' ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'both')
#' ts_prop_test(var1 = mtcars$am, var2 = mtcars$vs, alternative = 'all')
#'
#' # using groups
#' ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'less')
#' ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'greater')
#' ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'both')
#' ts_prop_grp(var = mtcars$am, group = mtcars$vs, alternative = 'all')
#'
#' # using sample size and proportions
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'less')
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'greater')
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'both')
#' ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'all')
#' @export
#'
ts_prop_test <- function(var1, var2,
  alternative = c('both', 'less', 'greater', 'all')) UseMethod('ts_prop_test')

#' @export
#'
ts_prop_test.default <- function(var1, var2,
  alternative = c('both', 'less', 'greater', 'all')) {


	n1 <- length(var1)
	n2 <- length(var2)
	y1 <- table(var1)[[2]]
	y2 <- table(var2)[[2]]
	phat1 <- round(y1 / n1, 4)
	phat2 <- round(y2 / n2, 4)
	phat <- sum(y1, y2) / sum(n1, n2)

	# test statistic
	num <- (phat1 - phat2)
	den1 <- phat * (1 - phat)
	den2 <- (1 / n1) + (1 / n2)
	den <- sqrt(den1 * den2)
	z <- round(num / den, 4)

	lt <- round(pnorm(z), 4)
	ut <- round(pnorm(z, lower.tail = FALSE), 4)
	tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('two-tail' = tt, 'lower-tail' = lt, 'upper-tail' = ut)
    } else if (alt == "greater") {
        sig = ut
    } else if (alt == "less"){
        sig = lt
    } else {
        sig = tt
    }

    # result
    result <- list(n1 = n1,
                   n2 = n2,
                   phat1 = phat1,
                   phat2 = phat2,
                   z = z,
    	             sig = sig,
                   alt = alt)

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
	ut <- 1 - pnorm(z)
	tt <- pnorm(z) * 2

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

    out <- list(n1    = n1,
                n2    = n2,
                phat1 = phat1,
                phat2 = phat2,
                z     = z,
                sig   = sig,
                alt   = alt)

    class(out) <- 'ts_prop_test'
    return(out)

}


#' @export
#' @rdname ts_prop_test
#'
ts_prop_calc <- function(n1, n2, p1, p2,
  alternative = c('both', 'less', 'greater', 'all')) {

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
	ut <- 1 - pnorm(z)
	tt <- pnorm(z) * 2

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

    out <- list(n1    = n1,
                n2    = n2,
                phat1 = phat1,
                phat2 = phat2,
                z     = z,
                sig   = sig,
                alt   = alt)

    class(out) <- 'ts_prop_test'
    return(out)

}
