#' @importFrom stats qnorm
#' @title One Sample Chi Square Test
#' @description Test whether a single categorical variable follows a
#' hypothesized population distribution.
#' @param x categorical variable
#' @param y expected proportion/frequencies
#' @param correct logical; if TRUE continuity correction is applied
#' @param conf.int confidence level
#' @return \code{os_chisqgof} returns an object of class \code{"os_chisqgof"}.
#' An object of class \code{"os_chisqgof"} is a list containing the
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
#' \item{c_low}{lower confidence limit for mean}
#' \item{c_up}{upper confidence limit for mean}
#'
#' @examples
#' os_chisqgof(hsb$female, c(100, 100))
#' os_chisqgof(hsb$race, c(50, 50, 50, 50))
#'
#' # continuity correction
#' os_chisqgof(hsb$female, c(100, 100), correct = TRUE)
#' @export
#'
os_chisqgof <- function(x, y, correct = FALSE, conf.int = 0.95) UseMethod('os_chisqgof')

#' @export
#'
os_chisqgof.default <- function(x, y, correct = FALSE, conf.int = 0.95) {
    len <- length(x)
	x1 <- as.factor(x)
	varname <- l(deparse(substitute(x)))
    x <- as.vector(table(x1))
    n <- length(x)
    df <- n - 1
    if (sum(y) == 1) {
        y <- length(x1) * y
    }

    # set correct to FALSE if k > 2
    if (df > 1 & correct == TRUE) {
        warning('Continuity correction is applicable only in case of binary categories.')
        correct <- FALSE
    }

    # continuity correction
    if (correct == TRUE) {
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

    # p value
    sig <- round(pchisq(chi, df, lower.tail = FALSE), 4)

    # confidence interval
    if (n == 2) {
        a <- (1 - conf.int) / 2
        z <- qnorm(a, lower.tail = F)
        p1 <- x[2] / len
        p2 <- x[1] / len
        cf <- z * sqrt((p1 * p2) / len)
        cl <- p1 - cf
        cu <- p1 + cf

    }

    if (n == 2) {
        result <- list(chisquare = chi, pvalue = sig, df = df, ssize = length(x1),
            names = levels(x1), level = nlevels(x1), obs = x, exp = y,
            deviation = format(dev, nsmall = 2), std = format(std, nsmall = 2),
            varname = varname, c_low = cl, c_up = cu)
    } else {
        result <- list(chisquare = chi, pvalue = sig, df = df, ssize = length(x1),
            names = levels(x1), level = nlevels(x1), obs = x, exp = y,
            deviation = format(dev, nsmall = 2), std = format(std, nsmall = 2),
            varname = varname)
    }

    class(result) <- 'os_chisqgof'
    return(result)
}

#' @export
#'
print.os_chisqgof <- function(x, ...) {
  print_os_chisqgof(x)
}
