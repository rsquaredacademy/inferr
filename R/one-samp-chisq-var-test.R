# generic
os_vartest <- function(x, var, alpha = 0.05, zapprox = FALSE, 
	alt = c('two', 'lower', 'upper', 'all')) UseMethod('os_vartest')

# default
os_vartest.default <- function(x, var, alpha = 0.05, zapprox = FALSE,
	alt = c('two', 'lower', 'upper', 'all')) {

	n <- length(x)
	df <- n - 1
	xbar <- mean(x)
	sum_x <- sum(x) ^ 2
	x2 <- sum(x ^ 2)
	sigma <- (x2 - (sum_x / n)) / df
	chi <- (df * sigma) / var

	# p values
	p_lower <- pchisq(chi, df)
	p_upper <- pchisq(chi, df, lower.tail = F)
	p_two <- pchisq(chi, df, lower.tail = F) * 2

	# confidence limits
	a <- alpha / 2
	al <- 1 - a
	tv <- df * sigma
	c_lwr <- tv / qchisq(al, df)
	c_upr <- tv / qchisq(a, df)

	if (zapprox) {

		if (n < 30) {
			stop('Sample size must be at least 30 for normal approximation.')
		}

		s <- sqrt(sigma)
		sqrvar <- sqrt(var)
		z <- (s - sqrvar) / (sqrvar / sqrt(2 * n))
		zp_lower <- pnorm(z)
		zp_upper <- pnorm(z, lower.tail = F)
		zp_two <- pnorm(z, lower.tail = F) * 2

		zv <- qnorm(a, lower.tail = F) / sqrt(2 * n)

		cz_lwr <- s / (1 + zv)
		cz_upr <- s / (1 - zv)

		result <- list(n = n, var = var, sigma = sigma, chi = chi, 
		df = df, p_lower = p_lower, p_upper = p_upper, p_two = p_two,
		c_lwr = c_lwr, c_upr = c_upr, cz_lwr = cz_lwr, cz_upr = cz_upr,
		zp_lower = zp_lower, zp_upper = zp_upper, zp_two = zp_two)

	} else {

		result <- list(n = n, var = var, sigma = sigma, chi = chi, 
		df = df, p_lower = p_lower, p_upper = p_upper, p_two = p_two,
		c_lwr = c_lwr, c_upr = c_upr)

	}	

	class(result) <- 'os_vartest'
	
	return(result)

}

# print method

# plot method

# test 
x <- c(5, 6, 4, 3, 11, 12, 9, 13, 6, 8)
