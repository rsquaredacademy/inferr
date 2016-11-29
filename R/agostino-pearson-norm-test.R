# d'agostino pearson omnibus normality test
omnibus_normtest <- function(x) UseMethod('omnibus_normtest')

# default
omnibus_normtest.default <- function(x) {

	zg1 <- os_skewtest(x)$z ^ 2
	zg2 <- os_kurtosistest(x)$z ^ 2
	chi <- zg1 + zg2
	pval <- pchisq(chi, 2, lower.tail = F) 

	result <- list(chi = chi, pval = pval)

	class(result) <- 'omnibus_normtest'

	return(result)
}