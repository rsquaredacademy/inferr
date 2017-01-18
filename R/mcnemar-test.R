#' @importFrom stats qnorm
#' @title McNemar Test
#' @description Test if the proportions of two dichotomous variables are
#' equal in the same population.
#' @param x 2 x 2 matrix or 2 x 2 table or numeric variable or factor variable
#' @param y numeric or factor variable
#' @return \code{mcnemar_test} returns an object of class \code{"mcnemar_test"}.
#' An object of class \code{"mcnemar_test"} is a list containing the
#' following components:
#'
#' \item{statistic}{chi square statistic}
#' \item{df}{degrees of freedom}
#' \item{pvalue}{p-value}
#' \item{exactp}{exact p-value}
#' \item{cstat}{continuity correction chi square statistic}
#' \item{cpvalue}{continuity correction p-value}
#' \item{kappa}{kappa coefficient}
#' \item{std_err}{asymptotic standard error}
#' \item{kappa_cil}{95\% lower confidence limit}
#' \item{kappa_ciu}{95\%upper confidence limit}
#' \item{cases}{cases}
#' \item{controls}{controls}
#' \item{ratio}{ratio of proportion with factor}
#' \item{odratio}{odds ratio}
#' \item{tbl}{two way table}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @seealso \code{\link[stats]{mcnemar.test}}
#' @examples
#' # test if the proportion of students in himath and hiread group is same
#' himath <- ifelse(hsb$math > 60, 1, 0)
#' hiread <- ifelse(hsb$read > 60, 1, 0)
#' mcnemar_test(table(himath, hiread))
#'
#' # using matrix
#' mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
#' @export
#'
mcnemar_test <- function(x, y = NULL) UseMethod('mcnemar_test')

#' @export
#'
mcnemar_test.default <- function(x, y = NULL) {

	if (is.null(y)) {

		if (!is.matrix(x)) {
			stop('x must be either a table or a matrix')
		}

		if (is.matrix(x)) {
			if (length(x) != 4) {
				stop('x must be a 2 x 2 matrix')
			}
		}

		dat <- x

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

	 retrieve <- matrix(c(1, 2, 2, 1), nrow = 2)
	        p <- dat[retrieve]
	test_stat <- ((p[1] - p[2]) ^ 2) / sum(p)
	       df <- nrow(dat) - 1
	   pvalue <- 1 - pchisq(test_stat, df)
	   exactp <- 2 * min(pbinom(dat[2], sum(dat[2], dat[3]), 0.5), pbinom(dat[3], sum(dat[2], dat[3]), 0.5))
	    cstat <- ((abs(p[1] - p[2]) - 1) ^ 2) / sum(p)
	  cpvalue <- 1 - pchisq(cstat, df)
	agreement <- sum(diag(dat)) / sum(dat)
	 expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
	    kappa <- (agreement - expected) / (1 - expected)
	  std_err <- serr(dat, kappa, expected)

	# confidence intervals
	    alpha <- 0.05
	 interval <- qnorm(1 - (alpha /2)) * std_err
	 ci_lower <- kappa - interval
	 ci_upper <- kappa + interval

	# proportions
	  dat_per <- dat / sum(dat)
	  row_sum <- rowSums(dat_per)
	  col_sum <- colSums(dat_per)
	 controls <- 1 - col_sum[2]
	    cases <- 1 - row_sum[2]
	    ratio <- cases / controls
 odds_ratio <- p[1] / p[2]

	result <- list(statistic = round(test_stat, 4),
		             df        = df,
		             pvalue    = round(pvalue, 4),
		             exactp    = round(exactp, 4),
		             cstat     = cstat,
		             cpvalue   = cpvalue,
		             kappa     = round(kappa, 4),
		             std_err   = round(std_err, 4),
		             kappa_cil = round(ci_lower, 4),
		             kappa_ciu = round(ci_upper, 4),
		             cases     = round(cases, 4),
		             controls  = round(controls, 4),
		             ratio     = round(ratio, 4),
		             odratio   = round(odds_ratio, 4),
		             tbl       = dat)

	class(result) <- 'mcnemar_test'
	return(result)

}

#' @export
#'
print.mcnemar_test <- function(x, ...) {
	print_mcnemar_test(x)
}
