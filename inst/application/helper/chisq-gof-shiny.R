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
#' @seealso \code{\link[stats]{chisq.test}}
#' @examples
#' chisq_gof(as.factor(hsb$race), c(20, 20, 20, 140))
#'
#' # apply continuity correction
#' chisq_gof(as.factor(hsb$race), c(20, 20, 20, 140), correct = TRUE)
#' @export
#'
chisq_gof_shiny <- function(data, x, y, correct = FALSE) UseMethod('chisq_gof_shiny')

#' @export
chisq_gof_shiny.default <- function(data, x, y, correct = FALSE) {



		if (!is.logical(correct)) {
			stop('correct must be either TRUE or FALSE')
		}

    varname <- x
         x1 <- data %>% select_(x) %>% `[[`(1)
          x2 <- as.vector(table(x1))
          n <- length(x2)

		if (length(y) != n) {
			stop('Length of y must be equal to the number of categories in x')
		}

    df <- n - 1
    if (sum(y) == 1) {
        y <- length(x1) * y
    }
    if ((df == 1) || (correct == TRUE)) {
        diff <- x2 - y - 0.5
         dif <- abs(x2 - y) - 0.5
        dif2 <- dif ^ 2
    		 dev <- round((diff / y) * 100, 2)
    		 std <- round(diff / sqrt(y), 2)
    		 chi <- round(sum(dif2 / y), 4)
    } else {
         dif <- x2 - y
        dif2 <- dif ^ 2
    		 dev <- round((dif / y) * 100, 2)
    		 std <- round(dif / sqrt(y), 2)
    		 chi <- round(sum(dif2 / y), 4)
    }

		sig <- round(pchisq(chi, df, lower.tail = FALSE), 4)

		result <- list(
			chisquare = chi,
			   pvalue = sig,
    	       df = df,
    	    ssize = length(x1),
    	    names = levels(x1),
    	    level = nlevels(x1),
    	      obs = x2,
    	      exp = y,
    	deviation = format(dev, nsmall = 2),
    	      std = format(std, nsmall = 2),
    	  varname = varname
			)

    class(result) <- 'chisq_gof_shiny'
    return(result)
}

#' @export
print.chisq_gof_shiny <- function(x, ...) {
	print_chisq_gof(x)
}


print_chisq_gof <- function(data) {

	cwidth <- max(nchar('Chi-Square'), nchar('DF'), nchar('Pr > Chi Sq'), nchar('Sample Size'))
	nwidth <- max(nchar(data$chisquare), nchar(data$df), nchar(data$pvalue), nchar(data$ssize))
	w1 <- sum(cwidth, nwidth, 6)
	lw <- max(nchar('Variable'), nchar(data$names))
	ow <- max(nchar('Observed'), nchar(data$obs))
	ew <- max(nchar('Expected'), nchar(data$exp))
	dw <- max(nchar('% Deviation'), nchar(data$deviation))
	rw <- max(nchar('Std. Residuals'), nchar(data$std))
	w <- sum(lw, ow, ew, dw, rw, 16)


	cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
	cat(rep("-", w1), sep = "", '\n')
	cat(format('Chi-Square', width = cwidth, justify = 'left'), formats(), format(data$chisquare, width = nwidth, justify = 'right'), '\n')
	cat(format('DF', width = cwidth, justify = 'left'), formats(), format(data$df, width = nwidth, justify = 'right'), '\n')
	cat(format('Pr > Chi Sq', width = cwidth, justify = 'left'), formats(), format(data$pvalue, width = nwidth, justify = 'right'), '\n')
	cat(format('Sample Size', width = cwidth, justify = 'left'), formats(), format(data$ssize, width = nwidth, justify = 'right'), '\n\n')
	cat(format(paste('Variable:', data$varname), width = w, justify = 'centre'), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fg('Category', lw), fs(), fg('Observed', ow), fs(), fg('Expected', ew), fs(), fg('% Deviation', dw), fs(), fg('Std. Residuals', rw), '\n')
	cat(rep("-", w), sep = "", '\n')
	for (i in seq_len(data$level)) {
		cat(fg(data$names[i], lw), fs(), fg(data$obs[i], ow), fs(), fg(data$exp[i], ew), fs(),
			fg(data$deviation[i], dw), fs(), fg(data$std[i], rw), '\n')
	}
	cat(rep("-", w), sep = "", '\n')

}
