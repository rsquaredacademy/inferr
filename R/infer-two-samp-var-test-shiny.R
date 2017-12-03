#' @importFrom stats complete.cases
#' @importFrom purrr map_dbl
#' @title Two Sample Variance Comparison Test Shiny
#' @description  \code{var_test} performs tests on the equality of standard
#' deviations (variances).
#' @param data a dataframe
#' @param variable1 character; name of sample 1 in data frame
#' @param variable2 character; name of sample 2 in data frame
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
#' # using two variables
#' # lower tail
#' var_test_shiny(hsb, 'read', 'write', alternative = 'less')
#'
#' # upper tail
#' var_test_shiny(hsb, 'read', 'write', alternative = 'greater')
#'
#' # all tails
#' var_test_shiny(hsb, 'read', 'write', alternative = 'all')
#'
#' @export
#'
var_test_shiny <- function(data, variable1, variable2, 
	alternative = c("less", "greater", "all")) UseMethod('var_test_shiny')

#' @export
#'
var_test_shiny.default <- function(data, variable1, variable2, 
	alternative = c("less", "greater", "all")) {

			variables <- data %>% select_(variable1, variable2)
			z   <- list(variables[[1]], variables[[2]])
			ln <- z %>% map_int(length)
			ly <- seq_len(length(z))

		  if (length(z) < 2) {
    		stop('Please specify at least two variables.', call. = FALSE)
    	}

		        out <- gvar(ln, ly)
		  variable  <- unlist(z)
		  group_var <- unlist(out)

	type <- match.arg(alternative)
	   k <- var_comp(variable, group_var)
	   lev <- c(variable1, variable2)
	   
	out <- list(f = k$f, lower = k$lower, upper = k$upper, vars = k$vars,
         avgs = k$avgs, sds = k$sds, ses = k$ses, avg = k$avg, sd = k$sd,
         se = k$se, n1 = k$n1, n2 = k$n2, lens = k$lens, len = k$len,
         lev = lev, type = type)

	class(out) <- 'var_test_shiny'
  return(out)

}

#' @export
#'
print.var_test_shiny <- function(x, ...) {
  print_var_test(x)
}
