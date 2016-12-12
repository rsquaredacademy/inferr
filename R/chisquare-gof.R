# data and expected count
gof <- function(x, y, correct = FALSE) UseMethod('gof')

gof.default <- function(x, y, correct = FALSE) {
	x1 <- as.factor(x)
	varname <- l(deparse(substitute(x)))
    x <- as.vector(table(x))
    n <- length(x)
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
    result <- list(Chisquare = chi,
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

    class(result) <- 'gof'
    return(result)
}

print.gof <- function(data, ...) {

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
