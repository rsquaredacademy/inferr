#' @importFrom stats complete.cases
#' @importFrom purrr map_dbl
#' @title Two sample variance comparison test
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
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'less')
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'greater')
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'all')
#'
#' # using two variables
#' var_test(hsb$read, hsb$write, alternative = 'less')
#' var_test(hsb$read, hsb$write, alternative = 'greater')
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

	suppressWarnings(
		if (is.na(group_var)) {
			name1 <- l(unlist(strsplit(deparse(substitute(c(variable, ...))), ','))[1])
			name2 <- unlist(strsplit(l(unlist(strsplit(deparse(substitute(c(variable, ...))), ','))[2]), ')'))[1]
			lev <- c(name1, name2)
		} else {
			if (!is.factor(group_var)) {
				group_var <- as.factor(group_var)
			}
			lev  <- levels(group_var)
		}
	)

	suppressWarnings(

		if (is.na(group_var)) {

			z   <- list(variable, ...)
			ln <- z %>% map_int(length)
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

	type     <- match.arg(alternative)
	comp     <- complete.cases(variable, group_var)
	cvar		 <- variable[comp]
	gvar     <- group_var[comp]

	   d     <- tibble(cvar, gvar)
	vals     <- tibble_stats(d, 'cvar', 'gvar')
	lass     <- tbl_stats(d, 'cvar')

	lens     <- vals[[2]] %>% map_int(1)
	vars     <- vals[[4]] %>% map_dbl(1)

	f        <- vars[1] / vars[2]
	n1       <- lens[1] - 1
	n2       <- lens[2] - 1
	lower    <- pf(f, n1, n2)
	upper    <- pf(f, n1, n2, lower.tail = FALSE)

	out <- list(f        = round(f, 4),
              lower    = round(lower, 4),
              upper    = round(upper, 4),
              vars     = round(vars, 2),
              avgs     = round((vals[[3]] %>% map_dbl(1)), 2),
              sds      = round((vals[[5]] %>% map_dbl(1)), 2),
              ses      = round((vals[[6]] %>% map_dbl(1)), 2),
              avg      = round(lass[2], 2),
              sd       = round(lass[3], 2),
              se       = round(lass[4], 2),
              n1       = n1,
              n2       = n2,
              lens     = lens,
              len      = lass[1],
              lev      = lev,
              type     = type)

	class(out) <- 'var_test'
  return(out)

}

#' @export
#'
print.var_test <- function(x, ...) {
  print_var_test(x)
}
