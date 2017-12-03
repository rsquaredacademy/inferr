#' @importFrom stats anova model.frame formula
#' @importFrom purrr map_int
#' @title Levene's test for equality of variances
#' @description  \code{levene_test} reports Levene's robust test statistic
#' for the equality of variances and the
#' two statistics proposed by Brown and Forsythe that replace the mean in
#' Levene's formula with alternative location estimators. The first alternative
#' replaces the mean with the median. The second alternative replaces
#' the mean with the 10% trimmed mean.
#' @param variable a numeric vector or formula or object of class \code{lm}
#' @param group_var a grouping variable
#' @param trim.mean trimmed mean
#' @param data a data frame
#' @param ... numeric vectors
#' @return \code{infer_levene_test} returns an object of class \code{"infer_levene_test"}.
#' An object of class \code{"infer_levene_test"} is a list containing the
#' following components:
#'
#' \item{bf}{Brown and Forsythe f statistic}
#' \item{p_bf}{p-value for Brown and Forsythe f statistic}
#' \item{lev}{Levene's f statistic}
#' \item{p_lev}{p-value for Levene's f statistic}
#' \item{bft}{Brown and Forsythe f statistic using trimmed mean}
#' \item{p_bft}{p-value for Brown and Forsythe f statistic using trimmed mean}
#' \item{avgs}{mean for each level of the grouping variable}
#' \item{sds}{standard deviations for each level of the grouping variable}
#' \item{avg}{combined mean}
#' \item{sd}{combined standard deviation}
#' \item{n}{number of observations}
#' \item{n_df}{numerator degrees of freedom}
#' \item{d_df}{denominator degrees of freedom}
#' \item{levs}{levels of the grouping variable}
#' \item{lens}{number of observations for each level of the grouping variable}
#' \item{type}{alternative hypothesis}
#' @section Deprecated Function:
#' \code{levene_test()} has been deprecated. Instead use \code{infer_levene_test()}.
#' @references
#' {Bland, M. 2000. An Introduction to Medical Statistics. 3rd ed. Oxford: Oxford University Press.}
#'
#' {Brown, M. B., and A. B. Forsythe. 1974. Robust tests for the equality of variances. Journal of the American Statistical
#' Association 69: 364–367.}
#'
#' {Carroll, R. J., and H. Schneider. 1985. A note on Levene’s tests for equality of variances. Statistics and Probability
#' Letters 3: 191–194.}
#' @examples
#' # using grouping variable
#' infer_levene_test(hsb$read, group_var = hsb$race)
#'
#' # using two variables
#' infer_levene_test(hsb$read, hsb$write, hsb$socst)
#'
#' # using model
#' m <- lm(read ~ female, data = hsb)
#' infer_levene_test(m)
#'
#' # using formula
#' infer_levene_test(as.formula(paste0('read ~ schtyp')), hsb)
#'
#' @export
#'
infer_levene_test <- function(variable, ...) UseMethod('infer_levene_test')

#' @export
#' @rdname levene_test
infer_levene_test.default <- function(variable, ..., group_var = NA,
	trim.mean = 0.1) {

	varname <- deparse(substitute(variable))

	suppressWarnings(

		if (is.na(group_var)) {

			if (is.data.frame(variable)) {
				z <- as.list(variable)
			} else {
				z <- list(variable, ...)
			}

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

	if (!is.factor(group_var)) {
		group_var <- as.factor(group_var)
	}
		 k <- lev_comp(variable, group_var, trim.mean)
	# comp <- complete.cases(variable, group_var)
	#    n <- length(comp)
 #     k <- nlevels(group_var)
	# cvar <- variable[comp]
	# gvar <- group_var[comp]
 #  lens <- tapply(cvar, gvar, length)
 #  avgs <- tapply(cvar, gvar, mean)
	#  sds <- tapply(cvar, gvar, sd)

	#  bf <- lev_metric(cvar, gvar, mean)
	# lev <- lev_metric(cvar, gvar, median)
	# bft <- lev_metric(cvar, gvar, mean, trim = trim.mean)

	out <- list(bf    = k$bf, p_bf  = k$p_bf, lev = k$lev, p_lev = k$p_lev,
              bft   = k$bft, p_bft = k$p_bft, avgs  = k$avgs, sds   = k$sds,
              avg   = k$avg, sd = k$sd, n = k$n, levs  = k$levs, n_df  = k$n_df,
              d_df  = k$d_df, lens  = k$lens)

	class(out) <- 'infer_levene_test'
  return(out)

}

#' @export
#' @rdname infer_levene_test
#' @usage NULL
#'
levene_test <- function(variable, ..., group_var = NA,
                        trim.mean = 0.1) {

    .Deprecated("infer_levene_test()")
    infer_levene_test(variable, ..., group_var,
                      trim.mean)

}

#' @export
#' @rdname infer_levene_test
#'
infer_levene_test.lm <- function(variable, ...) {
    infer_levene_test.formula(variable = formula(variable), data = model.frame(variable))
}

#' @export
#' @rdname infer_levene_test
#'
infer_levene_test.formula <- function(variable, data, ...) {
	dat       <- model.frame(variable, data)
	variable  <- dat[, 1]
	group_var <- dat[, 2]
	infer_levene_test.default(variable = variable, group_var = group_var)
}

#' @export
#'
print.infer_levene_test <- function(x, ...) {
  print_levene_test(x)
}
