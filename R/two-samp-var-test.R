#' @importFrom stats complete.cases
#' @importFrom purrr map_dbl
#' @title Two Sample Variance Comparison Test
#' @description  \code{var_test} performs tests on the equality of standard
#' deviations (variances).
#' @param variable a numeric vector
#' @param group_var a grouping variable
#' @param ... numeric vectors
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter.
#' @return \code{var_test} returns an object of class \code{"var_test"}.
#' An object of class \code{"var_test"} is a list containing the
#' following components:
#'
#' \item{f}{f statistic}
#' \item{lower}{lower one-sided p-value}
#' \item{upper}{upper one-sided p-value}
#' \item{two_tail}{two-sided p-value}
#' \item{vars}{variances for each level of the grouping variable}
#' \item{avgs}{means for each level of the grouping variable}
#' \item{sds}{standard deviations for each level of the grouping variable}
#' \item{ses}{standard errors for each level of the grouping variable}
#' \item{avg}{combined mean}
#' \item{sd}{combined standard deviation}
#' \item{se}{estimated combined standard error}
#' \item{n1}{numerator degrees of freedom}
#' \item{n2}{denominator degrees of freedom}
#' \item{lens}{number of observations for each level of grouping variable}
#' \item{len}{number of observations}
#' \item{lev}{levels of the grouping variable}
#' \item{type}{alternative hypothesis}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' # using grouping variable
#' # lower tail
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'less')
#'
#' # upper tail
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'greater')
#'
#' # all tails
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'all')
#'
#' # using two variables
#' # lower tail
#' var_test(hsb$read, hsb$write, alternative = 'less')
#'
#' # upper tail
#' var_test(hsb$read, hsb$write, alternative = 'greater')
#'
#' # all tails
#' var_test(hsb$read, hsb$write, alternative = 'all')
#'
#' @export
#'
var_test <- function(variable, ..., group_var = NA,
	alternative = c("less", "greater", "all")) UseMethod('var_test')

#' @export
#'
var_test.default <- function(variable, ..., group_var = NA,
	alternative = c("less", "greater", "all")) {

	if (!is.na(group_var)) {
		if (nlevels(as.factor(group_var)) != 2) {
			stop('group_var must be a binary factor variable.', call. = FALSE)
		}
	}

	suppressWarnings(
		if (is.na(group_var)) {
			name1 <- l(unlist(strsplit(deparse(substitute(c(variable, ...))), ','))[1])
			name2 <- unlist(strsplit(l(unlist(strsplit(deparse(substitute(c(variable, ...))), ','))[2]), ')'))[1]
			lev <- c(name1, name2)
		} else {
			if (!is.factor(group_var)) {
				group_var <- as.factor(group_var)
			}
			if (nlevels(group_var) > 2) {
				stop('Specify a binary factor variable as input for group_var.', call. = FALSE)
			}
			lev  <- levels(group_var)
		}
	)

	suppressWarnings(

		if (is.na(group_var)) {

			z   <- list(variable, ...)
			ln <- z %>% map_int(length)
			if (ln > 2) {
				stop('Only 2 variables can be specified.', call. = FALSE)
			}
			ly <- seq_len(length(z))

		  if (length(z) < 2) {
    		stop('Please specify at least two variables.', call. = FALSE)
    	}

		        out <- gvar(ln, ly)
		  variable  <- unlist(z)
		  group_var <- unlist(out)

		} else {

    	if (length(variable) != length(group_var)) {
    		stop('Length of variable and group_var do not match.', call. = FALSE)
    	}

    }

	)

	type <- match.arg(alternative)
	   k <- var_comp(variable, group_var)
	   
	out <- list(f = k$f, lower = k$lower, upper = k$upper, vars = k$vars,
         avgs = k$avgs, sds = k$sds, ses = k$ses, avg = k$avg, sd = k$sd,
         se = k$se, n1 = k$n1, n2 = k$n2, lens = k$lens, len = k$len,
         lev = lev, type = type)

	class(out) <- 'var_test'
  return(out)

}

#' @export
#'
print.var_test <- function(x, ...) {
  print_var_test(x)
}
