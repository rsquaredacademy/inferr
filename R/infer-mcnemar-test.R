#' @importFrom stats qnorm
#' @title McNemar Test
#' @description Test if the proportions of two dichotomous variables are
#' equal in the same population.
#' @param x 2 x 2 matrix or 2 x 2 table or numeric variable or factor variable
#' @param y numeric or factor variable
#' @return \code{infer_mcnemar_test} returns an object of class \code{"infer_mcnemar_test"}.
#' An object of class \code{"infer_mcnemar_test"} is a list containing the
#' following components:
#'
#' \item{statistic}{chi square statistic}
#' \item{df}{degrees of freedom}
#' \item{pvalue}{p-value}
#' \item{exactp}{exact p-value}
#' \item{cstat}{continuity correction chi square statistic}
#' \item{cpvalue}{continuity correction p-value}
#' \item{kappa}{kappa coefficient; measure of interrater agreement}
#' \item{std_err}{asymptotic standard error}
#' \item{kappa_cil}{95\% kappa lower confidence limit}
#' \item{kappa_ciu}{95\% kappa upper confidence limit}
#' \item{cases}{cases}
#' \item{controls}{controls}
#' \item{ratio}{ratio of proportion with factor}
#' \item{odratio}{odds ratio}
#' \item{tbl}{two way table}
#' @section Deprecated Function:
#' \code{mcnermar_test()} has been deprecated. Instead use
#' \code{infer_mcnemar_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @seealso \code{\link[stats]{mcnemar.test}}
#' @examples
#' # test if the proportion of students in himath and hiread group is same
#' himath <- ifelse(hsb$math > 60, 1, 0)
#' hiread <- ifelse(hsb$read > 60, 1, 0)
#' infer_mcnemar_test(table(himath, hiread))
#'
#' # using matrix
#' infer_mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
#' @export
#'
infer_mcnemar_test <- function(x, y = NULL) UseMethod('infer_mcnemar_test')

#' @export
#'
infer_mcnemar_test.default <- function(x, y = NULL) {

	if (is.null(y)) {

		dat <- mcdata(x, y)

	} else {

		if (length(x) != length(y)) {
			stop('x and y should be of the same length')
		}

		if ((!is.numeric(x) & !is.numeric(y)) &
			 (!is.factor(x) & !is.factor(y))) {
			 stop('x and y must be either numeric or factor')
		}

		dat <- table(x, y)

	}

  k <- mccomp(dat)

	result <- list(statistic = k$statistic, df = k$df, pvalue = k$pvalue,
		exactp = k$exactp, cstat = k$cstat, cpvalue = k$cpvalue, kappa = k$kappa,
    std_err = k$std_err, kappa_cil = k$kappa_cil, kappa_ciu = k$kappa_ciu,
    cases = k$cases, controls = k$controls, ratio = k$ratio,
    odratio = k$odratio, tbl = dat)

	class(result) <- 'infer_mcnemar_test'
	return(result)

}

#' @export
#' @rdname infer_mcnemar_test
#' @usage NULL
#'
mcnemar_test <- function(x, y = NULL) {

    .Deprecated("infer_mcnemar_test()")
    infer_mcnemar_test(x, y)

}

#' @export
#'
print.infer_mcnemar_test <- function(x, ...) {
	print_mcnemar_test(x)
}
