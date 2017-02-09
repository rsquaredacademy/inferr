#' @title Cochran Q Test
#' @description Test if the proportions of 3 or more dichotomous variables are
#' equal in the same population.
#' @param x a data frame/vector
#' @param ... numeric vectors
#' @return \code{cochran_test} returns an object of class \code{"cochran_test"}.
#' An object of class \code{"cochran_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{df}{degrees of freedom}
#' \item{q}{cochran's q statistic}
#' \item{pvalue}{p-value}
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' cochran_test(exam)
#' @export
#'
cochran_test <- function(x, ...) UseMethod('cochran_test')

#' @export
cochran_test.default <- function(x, ...) {

	data <- coch_data(x, ...)
	
	if (ncol(data) < 3) {
		stop('Please specify at least 3 variables.')
	}

	if (any(sapply(lapply(data, as.factor), nlevels) > 2)) {
		stop('Please specify dichotomous/binary variables only.')
	}

	k <- cochran_comp(data)		
	result <- list(n = k$n, df = k$df, q = k$q, pvalue = k$pvalue)
	class(result) <- 'cochran_test'
	return(result)

}

#' @export
#'
print.cochran_test <- function(x, ...) {
	print_cochran_test(x)
}
