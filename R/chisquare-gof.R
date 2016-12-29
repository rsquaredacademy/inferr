#' @title Chi Square Goodness of Fit Test
#' @description Test whether the observed proportions for a categorical variable
#' differ from hypothesized proportions
#' @param x categorical variable
#' @param y expected proportions
#' @param correct logical; if TRUE continuiuty correction is applied
#' @return \code{chisq_gof} returns an object of class \code{"chisq_gof"}.
#' An object of class \code{"chisq_gof"} is a list containing the
#' following components:
#'
#' \item{chisquare}{chi square statistic}
#' \item{pvalue}{p-value}
#' \item{df}{chi square degrees of freedom}
#' \item{ssize}{number of observations}
#' \item{names}{levels of \code{x}}
#' \item{level}{number of levels of \code{x}}
#' \item{obs}{observed frequency/proportion}
#' \item{exp}{expected frequency/proportion}
#' \item{deviation}{deviation of observed from frequency}
#' \item{std}{standardized residuals}
#' \item{varname}{name of categorical variable}
#'
#' @examples
#' chisq_gof(hsb$race, c(20, 20, 20 , 140))
#' chisq_gof(hsb$race, c(20, 20, 20 , 140), correct = TRUE)
#' @export
#'
chisq_gof <- function(x, y, correct = FALSE) UseMethod('chisq_gof')

#' @export
chisq_gof.default <- function(x, y, correct = FALSE) {

		if (!is.numeric(y)) {
			stop('y must be numeric')
		}

		if (!is.logical(correct)) {
			stop('correct must be either TRUE or FALSE')
		}

		x1 <- as.factor(x)
		varname <- l(deparse(substitute(x)))
    x <- as.vector(table(x))
    n <- length(x)

		if (length(y) != n) {
			stop('Length of y must be equal to the number of categories in x')
		}

    df <- n - 1
    if (sum(y) == 1) {
        y <- length(x1) * y
    }
    if ((df == 1) || (correct == TRUE)) {
        diff <- x - y - 0.5
        dif <- abs(x - y) - 0.5
        dif2 <- dif ^ 2
    		dev <- round((diff / y) * 100, 2)
    		std <- round(diff / sqrt(y), 2)
    		chi <- round(sum(dif2 / y), 4)
    } else {
        dif <- x - y
        dif2 <- dif ^ 2
    		dev <- round((dif / y) * 100, 2)
    		std <- round(dif / sqrt(y), 2)
    		chi <- round(sum(dif2 / y), 4)
    }
    sig <- round(1 - pchisq(chi, df), 4)
    result <- list(chisquare = chi,
    							 pvalue = sig,
    					 		 df = df,
    					 		 ssize = length(x1),
    					 		 names = levels(x1),
    					 		 level = nlevels(x1),
    					 		 obs = x,
    					 		 exp = y,
    					 		 deviation = format(dev, nsmall = 2),
    					 		 std = format(std, nsmall = 2),
    					 		 varname = varname)

    class(result) <- 'chisq_gof'
    return(result)
}

#' @export
print.chisq_gof <- function(x, ...) {
	print_chisq_gof(x)
}
