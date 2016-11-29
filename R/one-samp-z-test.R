# with variable
os_ztestc <- function(data, mu, sigma, alpha = 0.05,
	alt = c('two', 'lower', 'upper', 'all')) {

	n <- length(data)
	xbar <- mean(data)

	os_ztest.default(n, xbar, mu, sigma)

}

# generic
os_ztest <- function(n, xbar, mu, sigma, alpha = 0.05,
	alt = c('two', 'lower', 'upper', 'all')) UseMethod('os_ztest')

# default
os_ztest.default <- function(n, xbar, mu, sigma, alpha = 0.05,
	alt = c('two', 'lower', 'upper', 'all')) {

	se <- sigma / sqrt(n)
	z <- (xbar - mu) / se

	# p values
	p_lower <- pnorm(z)
	p_upper <- pnorm(z, lower.tail = F)
	p_two <- pnorm(abs(z), lower.tail = F) * 2

	# confidence limits
	a <- 1 - (alpha / 2)
	pv <- qnorm(a) * se
	c_lwr <- xbar - pv
	c_upr <- xbar + pv

	result <- list(n = n, xbar = xbar, mu = mu, sigma = sigma, z = z, se = se, 
		p_lower = p_lower, p_upper = p_upper, p_two = p_two, c_lwr = c_lwr, 
		c_upr = c_upr)

	class(result) <- 'os_ztest'
	
	return(result)

}

# print method

# plot method

