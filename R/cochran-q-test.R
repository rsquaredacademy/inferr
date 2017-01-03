#' @importFrom magrittr %>%
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
#' @seealso \code{\link[RVAideMemoire]{cochran.qtest}}
#' @examples
#' cochran_test(exam)
#' @export
#'
cochran_test <- function(x, ...) UseMethod('cochran_test')

#' @export
cochran_test.default <- function(x, ...) {

	if (is.data.frame(x)) {
		data <- x %>%
		    lapply(as.numeric) %>%
		    as.data.frame() %>%
		    `-`(1)
	} else {
		data <- cbind(x, ...) %>%
		    apply(2, as.numeric) %>%
		    `-`(1) %>%
		    as.data.frame()

	}
		if (ncol(data) < 3) {
			stop('Please specify at least 3 variables.')
		}

		if (any(sapply(lapply(data, as.factor), nlevels) > 2)) {
			stop('Please specify dichotomous/binary variables only.')
		}


	n 	   <- nrow(data)
	k      <- ncol(data)
	df     <- k - 1
	cs     <- sums(data)
	q      <- coch(k, cs$cls_sum, cs$cl, cs$g, cs$gs_sum)
	pvalue <- 1 - pchisq(q, df)

	result <- list(n      = n,
		             df     = df,
		             q      = q,
		             pvalue = round(pvalue, 4))

	class(result) <- 'cochran_test'
	return(result)

}

print.cochran_test <- function(x, ...) {
	print_cochran_test(x)
}
