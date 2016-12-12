cochran_test <- function(x, ...) UseMethod('cochran_test')

cochran_test.default <- function(x, ...) {

	if (is.data.frame(x)) {
		data <- x
	} else {
		data <- cbind(x, ...)
		if (ncol(data) < 3) {
			stop('Please specify at least 3 variables.')
		}
		if (any(sapply(lapply(data, as.factor), nlevels)) > 2) {
			stop('Please specify dichotomous/binary variables only.')
		}
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
		             pvalue = pvalue)

	class(result) <- 'cochran_test'
	return(result)

}
