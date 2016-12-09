#' @importFrom stats complete.cases
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
#' @examples
#' # using grouping variable
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'less')
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'greater')
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'both')
#' var_test(mtcars$mpg, group_var = mtcars$vs, alternative = 'all')
#'
#' # using two variables
#' os_vartest(mtcars$mpg, mtcars$qsec, alternative = 'less')
#' os_vartest(mtcars$mpg, mtcars$qsec, alternative = 'greater')
#' os_vartest(mtcars$mpg, mtcars$qsec, alternative = 'both')
#' os_vartest(mtcars$mpg, mtcars$qsec, alternative = 'all')
#'
#' @export
#'
var_test <- function(variable, ..., group_var = NA,
	alternative = c("both", "less", "greater", "all")) UseMethod('var_test')

#' @export
#'
var_test.default <- function(variable, ..., group_var = NA,
	alternative = c("both", "less", "greater", "all")) {

	suppressWarnings(

		if (is.na(group_var)) {

			z   <- list(variable, ...)
		  ln  <- lapply(z, length)
		  ly  <- length(z)

		  if (ly < 2) {
    		stop('Please specify at least two variables.', call. = FALSE)
    	}

		  out <- list()

		  for (i in seq_len(ly)) {
		    out[[i]] <- as.factor(rep(i, ln[i]))
		  }

		  variable  <- unlist(z)
		  group_var <- unlist(out)

		} else {

    	if (length(variable) != length(group_var)) {
    		stop('Length of variable and group_var do not match.', call. = FALSE)
    	}

    }

	)

	if (!is.factor(group_var)) {
		group_var <- as.factor(group_var)
	}

	lev      <- levels(group_var)
	type     <- match.arg(alternative)
	comp     <- complete.cases(variable, group_var)
	vars     <- tapply(variable[comp], group_var[comp], var)
	lens     <- tapply(variable[comp], group_var[comp], length)
	avgs     <- tapply(variable[comp], group_var[comp], mean)
	sds      <- tapply(variable[comp], group_var[comp], sd)
	ses      <- sds / sqrt(lens)
	len      <- length(variable)
	avg      <- mean(variable)
	sd       <- sd(variable)
	se       <- sd / sqrt(len)
	f        <- as.vector(vars[1] / vars[2])
	n1       <- lens[1] - 1
	n2       <- lens[2] - 1
	lower    <- pf(f, n1, n2)
	upper    <- 1 - pf(f, n1, n2)
	two_tail <- pf(f, n1, n2) * 2

	out <- list(f        = round(f, 4),
              lower    = round(lower, 4),
              upper    = round(upper, 4),
              two_tail = round(two_tail, 4),
              vars     = round(vars, 2),
              avgs     = round(avgs, 2),
              sds      = round(sds, 2),
              ses      = round(ses, 2),
              avg      = round(avg, 2),
              sd       = round(sd, 2),
              se       = round(se, 2),
              n1       = n1,
              n2       = n2,
              lens     = lens,
              len      = len,
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
