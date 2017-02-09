#' @importFrom stats qchisq
#' @title One Sample Variance Comparison Test
#' @description  \code{os_vartest} performs tests on the equality of standard
#' deviations (variances).It tests that the standard deviation of \code{x} is
#' \code{sd}.
#' @param x a numeric vector
#' @param sd hypothesised standard deviation
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{os_vartest} returns an object of class \code{"os_vartest"}.
#' An object of class \code{"os_vartest"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{sd}{hypothesised standard deviation of \code{x}}
#' \item{sigma}{observed standard deviation}
#' \item{se}{estimated standard error}
#' \item{chi}{chi-square statistic}
#' \item{df}{degrees of freedom}
#' \item{p_lower}{lower one-sided p-value}
#' \item{p_upper}{upper one-sided p-value}
#' \item{p_two}{two-sided p-value}
#' \item{xbar}{mean of \code{x}}
#' \item{c_lwr}{lower confidence limit of standard deviation}
#' \item{c_upr}{upper confidence limit of standard deviation}
#' \item{var_name}{name of \code{x}}
#' \item{conf}{confidence level}
#' \item{type}{alternative hypothesis}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' # lower tail
#' os_vartest(mtcars$mpg, 5, alternative = 'less')
#'
#' # upper tail
#' os_vartest(mtcars$mpg, 5, alternative = 'greater')
#'
#' # both tails
#' os_vartest(mtcars$mpg, 5, alternative = 'both')
#'
#' # all tails
#' os_vartest(mtcars$mpg, 5, alternative = 'all')
#' @export
#'
os_vartest <- function(x, sd, confint = 0.95,
	alternative = c('both', 'less', 'greater', 'all'), ...) UseMethod('os_vartest')

#' @export
#'
os_vartest.default <- function(x, sd, confint = 0.95,
	alternative = c('both', 'less', 'greater', 'all'), ...) {

	if (!is.numeric(x)) {
		stop('x must be numeric')
	}

	if (!is.numeric(sd)) {
		stop('sd must be numeric')
	}

	if (!is.numeric(confint)) {
		stop('confint must be numeric')
	}

	   type <- match.arg(alternative)
	varname <- l(deparse(substitute(x)))
	      k <- osvar_comp(x, sd, confint)

	result <- list(n = k$n, sd = k$sd, sigma = k$sigma, se = k$se, chi = k$chi, 
		df = k$df, p_lower = k$p_lower, p_upper = k$p_upper, p_two = k$p_two, 
		xbar = k$xbar, c_lwr = k$c_lwr, c_upr = k$c_upr, var_name = varname, 
		conf = k$conf, type = type)

	class(result) <- 'os_vartest'
	return(result)

}

#' @export
#'
print.os_vartest <- function(x, ...) {
  print_os_vartest(x)
}
