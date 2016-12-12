# to do
# 1. contrast in case k > 2
#   1.1 1 category vs all others
#   1.2 collapsing levels
#   1.3 just two categories
# 2. tune function to accept a matrix/table/vector
# 3. test theoretical distribution

# data and expected count
os_chisqgof <- function(x, y, correct = FALSE, conf.int = 0.95) UseMethod('os_chisqgof')

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
        result <- list(Chisquare = chi, pvalue = sig, df = df, ssize = length(x1),
            names = levels(x1), level = nlevels(x1), obs = x, exp = y,
            deviation = format(dev, nsmall = 2), std = format(std, nsmall = 2),
            varname = varname, c_low = cl, c_up = cu)
    } else {
        result <- list(Chisquare = chi, pvalue = sig, df = df, ssize = length(x1),
            names = levels(x1), level = nlevels(x1), obs = x, exp = y,
            deviation = format(dev, nsmall = 2), std = format(std, nsmall = 2),
            varname = varname)
    }

    class(result) <- 'os_chisqgof'

    return(result)
}

print.os_chisqgof <- function(data, ...) {

	cwidth <- max(nchar('Chi-Square'), nchar('DF'), nchar('Pr > Chi Sq'), nchar('Sample Size'))
	nwidth <- max(nchar(data$Chisquare), nchar(data$df), nchar(data$pvalue), nchar(data$ssize))
	w1 <- sum(cwidth, nwidth, 6)
	lw <- max(nchar('Variable'), nchar(data$names))
	ow <- max(nchar('Observed'), nchar(data$obs))
	ew <- max(nchar('Expected'), nchar(data$exp))
	dw <- max(nchar('% Deviation'), nchar(data$deviation))
	rw <- max(nchar('Std. Residuals'), nchar(data$std))
	w <- sum(lw, ow, ew, dw, rw, 16)


	cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
	cat(rep("-", w1), sep = "", '\n')
	cat(format('Chi-Square', width = cwidth, justify = 'left'), formats(), format(data$Chisquare, width = nwidth, justify = 'right'), '\n')
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
