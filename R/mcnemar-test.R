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
#' mcnemar_test(matrix(c(172, 7, 6, 15), nrow = 2))
#' mcnemar_test(matrix(c(15, 7, 6, 172), nrow = 2))
#' mcnemar_test(table(hsb$female, hsb$schtyp))
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

	# exact p value
	exactp <- 2 * pbinom(dat[3], sum(dat[2], dat[3]), 0.5)

	# continuity correction
	cstat <- ((abs(p[1] - p[2]) - 1) ^ 2) / sum(p)
	cpvalue <- 1 - pchisq(cstat, df)

	# kappa coefficeints
	agreement <- sum(diag(dat)) / sum(dat)

	# expected agreement
	expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)

	# kappa
	kappa <- (agreement - expected) / (1 - expected)

	# variance
	dat_per <- dat / sum(dat)
	row_sum <- rowSums(dat_per)
	row_sum[3] <- sum(row_sum)
	col_sum <- colSums(dat_per)
	dat_per <- rbind(dat_per, col_sum)
	dat_per <- cbind(dat_per, row_sum)
	d1 <- dim(dat_per)
	dat_per[d1[1], d1[2]] <- 1.0
	diagonal <- diag(dat_per)
	a <- diagonal[1] * (1 - (row_sum[1] + col_sum[1]) * (1 - kappa)) ^ 2 +
	    diagonal[2] * (1 - (row_sum[2] + col_sum[2]) * (1 - kappa)) ^ 2

	x1 <- dat_per[lower.tri(dat_per)][1]
	x2 <- dat_per[upper.tri(dat_per)][1]
	b <- ((1 - kappa) ^ 2) * ((x1 * (row_sum[1] + col_sum[2]) ^ 2) +
	    (x2 * (row_sum[2] + col_sum[1]) ^ 2))

	c <- ((kappa) - expected * (1 - kappa)) ^ 2
	variance <- ((a + b -c) / ((1 - expected) ^ 2)) / sum(dat)
	std_err <- sqrt(variance)

	# confidence intervals
	alpha <- 0.05
	interval <- qnorm(1 - (alpha /2)) * std_err
	ci_lower <- kappa - interval
	ci_upper <- kappa + interval

	# proportions
	controls   <- 1 - col_sum[2]
	cases      <- 1 - row_sum[2]
	ratio      <- cases / controls
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
