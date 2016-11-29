# generic
os_ttest <- function(x, mu, alpha = 0.05,
	alt = c('two', 'lower', 'upper', 'all')) UseMethod('os_ttest')

# default
os_ttest.default <- function(x, mu, alpha = 0.05,
	alt = c('two', 'lower', 'upper', 'all')) {

	n <- length(x)
	df <- n - 1
	xbar <- mean(x)
	sum_x <- sum(x) ^ 2
	x2 <- sum(x ^ 2)
	sigma <- sqrt((x2 - (sum_x / n)) / df)
	se <- sigma / sqrt(n)
	t <- (xbar - mu) / se

	# p values
	p_lower <- pt(t, df)
	p_upper <- pt(t, df, lower.tail = F)
	p_two <- pt(abs(t), df, lower.tail = F) * 2
	
	# confidence limits
	a <- 1 - (alpha / 2)
	tv <- qt(a, df) * se
	c_lwr <- xbar - tv
	c_upr <- xbar + tv

	result <- list(n = n, xbar = xbar, mu = mu, sigma = sigma, se = se, 
		t = t, df = df, p_lower = p_lower, p_upper = p_upper, p_two = p_two,
		c_lwr = c_lwr, c_upr = c_upr)
	class(result) <- 'os_ttest'
	return(result)

}

# print method

# plot method

# test 
x <- c(9, 10, 8, 4, 8, 3, 0, 10, 15, 9)
