# test data
e <- c(rep(0, 3), rep(5, 4), rep(10, 3))
f <- c(0, 1, 1, 9, 9, rep(10, 5))
g <- c(rep(0, 5), 1, 1, 9, 9, 10)

skewness <- function(x) {
    n <- length(x)
    sum_x <- sum(x)
    x3 <- sum(x ^ 3)
    x2 <- sum(x ^ 2)
    sum_x3 <- sum_x ^ 3
    result <- ((n * x3) - (3 * sum_x * x2) + (2 * (sum_x3 / n))) / ((n - 1) * (n - 2))
    return(result)
}


# generic
os_skewtest <- function(x, alpha = 0.05, 
	alt = c('two', 'lower', 'upper', 'all')) UseMethod('os_skewtest')

# default
os_skewtest.default <- function(x, alpha = 0.05, 
	alt = c('two', 'lower', 'upper', 'all')) {

	n <- length(x)

	# sample standard deviation
	ssd <- sd(x)

	# skewness
	m3 <- skewness(x)
	g1 <- m3 / (ssd ^ 3)
	rb1 <- ((n - 2) * g1) / sqrt(n * (n - 1))

	# test statistic
	a <- rb1 * sqrt(((n + 1) * (n + 3)) / (6 * (n - 2)))
	b <- (3 * ((n ^ 2) + (27 * n) - 70) * (n + 1) * (n + 3)) / ((n - 2) * (n + 5 ) * (n + 7) * (n + 9))
	c <- sqrt(2 * (b - 1)) - 1
	d <- sqrt(c)
	e <- 1 / sqrt(log(d))
	f <- a / sqrt(2 / (c - 1))
	z <- e * log(f + sqrt((f ^ 2) + 1)) 

	# p values
	p_lower <- pnorm(z)
	p_upper <- pnorm(z, lower.tail = F)
	p_two <- pnorm(abs(z), lower.tail = F) * 2

	result <- list(n = n, sd = ssd, m3 = m3, g1 = g1, rootb1 = rb1, 
		a = a , b = b, c = c, d = d, e = e, f = f,
		z = z, p_lower = p_lower, p_upper = p_upper, p_two = p_two)

	class(result) <- 'os_skewtest'
	
	return(result)
}


