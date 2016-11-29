# test data
h <- c(2, 7, rep(8, 3), rep(9, 3), rep(10, 4), rep(11, 3), rep(12, 3), 13, 18)
i <- c(0, 1, rep(3, 2), rep(5, 2), rep(8, 2), rep(10, 4), rep(12, 2), rep(15, 2), rep(17, 2), 19, 20)

kurtosis <- function(x) {
    n <- length(x)
    n2 <- n ^ 2
    n3 <- n ^ 3
    sum_x <- sum(x)
    x3 <- sum(x ^ 3)
    x2 <- sum(x ^ 2)
    x4 <- sum(x ^ 4)
    num1 <- ((n3 + n2) * x4)
    num2 <- (4 * (n2 + n) * x3 * sum_x)
    num3 <- (3 * (n2 - n) * (x2 ^ 2))
    num4 <- (12 * n * x2 * (sum_x ^ 2))
    num5 <- (6 * (sum_x ^ 4))
    den1 <- (n * (n - 1) * (n - 2)  * (n - 3))
    result <- (num1 - num2 - num3 + num4 - num5) / den1
    return(result)
}


# generic
os_kurtosistest <- function(x, alt = c('two', 'lower', 'upper', 'all')) UseMethod('os_kurtosistest')

# default
os_kurtosistest.default <- function(x, alt = c('two', 'lower', 'upper', 'all')) {

	n <- length(x)

	# sample standard deviation
	ssd <- sd(x)

	# skewness
	k4 <- kurtosis(x)
	g2 <- k4 / (ssd ^ 4)
	b2 <- (((n - 2) * (n - 3) * g2) / ((n + 1) * (n - 1))) + ((3 * (n - 1)) / (n + 1))

	# test statistic
	g <- (24 * n * (n - 2) * (n - 3)) / (((n + 1) ^ 2) * (n + 3) * (n + 5))
	h <- ((n - 2) * (n - 3) * abs(g2)) / ((n + 1) * (n - 1) * sqrt(g))
	j1 <- 6 * ((n ^ 2) - (5 * n) + 2) 
	j2 <- (n + 7) * (n + 9)
	j3 <- 6 * (n + 3) * (n + 5)
	j4 <- n * (n - 2) * (n - 3)
	j <- (j1 / j2) * (sqrt(j3 / j4))
	k <- 6 + ((8 / j) * ((2 / j) + sqrt(1 + (4 / (j ^ 2)))))
	l <- (1 - (2 / k)) / (1 + (h * sqrt(2 / (k - 4))))
	z <- (1 - (2 / (9 * k)) - (l ^ (1 / 3))) / sqrt(2 / (9 * k))

	# p values
	p_lower <- pnorm(z)
	p_upper <- pnorm(z, lower.tail = F)
	p_two <- pnorm(abs(z), lower.tail = F) * 2

	result <- list(n = n, sd = ssd, k4 = k4, g2 = g2, b2 = b2, 
		g = g, h = h, j = j, k = k, l = l,
		z = z, p_lower = p_lower, p_upper = p_upper, p_two = p_two)

	class(result) <- 'os_kurtosistest'
	
	return(result)
}


