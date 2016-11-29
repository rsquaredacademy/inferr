# two sample proportion test
ts_prop_1 <- function(var1, var2, dif = 0, alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all')) {

	# diff 
	p_diff <- dif

	# length
	n1 <- length(var1)
	n2 <- length(var2)

	# y's
	y1 <- table(var1)[[2]]
	y2 <- table(var2)[[2]]

	# phats
	phat1 <- y1 / n1
	phat2 <- y2 / n2
	phat <- sum(y1, y2) / sum(n1, n2)

	# test statistic
	num <- (phat1 - phat2) - p_diff
	den1 <- phat * (1 - phat)
	den2 <- (1 / n1) + (1 / n2)
	den <- sqrt(den1 * den2)
	z <- num / den

	# lower tail 
	lt <- pnorm(z)

	# upper tail
	ut <- 1 - pnorm(z)

	# two tail
	tt <- pnorm(z) * 2

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('two-tail' = tt, 'lower-tail' = lt, 'upper-tail' = ut)
    } else if (alt == "upper-tail") {
        sig = ut
    } else if (alt == "lower-tail"){
        sig = lt
    } else {
        sig = tt
    }

    # result
    out <- list(n1 = n1, n2 = n2, phat1 = phat1, phat2 = phat2, z = z, sig = sig)

    return(out)

}


ts_prop_2 <- function(var, group, dif = 0, alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all')) {

	# diff 
	p_diff <- dif

	# length
	n <- tapply(var, group, length)
	n1 <- n[[1]]
	n2 <- n[[2]]

	# y's
	y <- tapply(var, group, table)
	y1 <- y[[1]][[2]]
	y2 <- y[[2]][[2]]


	# phats
	phat1 <- y1 / n1
	phat2 <- y2 / n2
	phat <- sum(y1, y2) / sum(n1, n2)

	# test statistic
	num <- (phat1 - phat2) - p_diff
	den1 <- phat * (1 - phat)
	den2 <- (1 / n1) + (1 / n2)
	den <- sqrt(den1 * den2)
	z <- num / den

	# lower tail 
	lt <- pnorm(z)

	# upper tail
	ut <- 1 - pnorm(z)

	# two tail
	tt <- pnorm(z) * 2

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('two-tail' = tt, 'lower-tail' = lt, 'upper-tail' = ut)
    } else if (alt == "upper-tail") {
        sig = ut
    } else if (alt == "lower-tail"){
        sig = lt
    } else {
        sig = tt
    }

    # result
    out <- list(n1 = n1, n2 = n2, phat1 = phat1, phat2 = phat2, z = z, sig = sig)

    return(out)

}


# two sample test of proportion calculator
ts_prop_calc <- function(n1, n2, p1, p2, dif = 0, alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all')) {

	# diff 
	p_diff <- dif

	# length
	n1 <- n1
	n2 <- n2

	# phats
	phat1 <- p1
	phat2 <- p2
	phat <- sum(y1, y2) / sum(n1, n2)

	# test statistic
	num <- (phat1 - phat2) - p_diff
	den1 <- phat * (1 - phat)
	den2 <- (1 / n1) + (1 / n2)
	den <- sqrt(den1 * den2)
	z <- num / den

	# lower tail 
	lt <- pnorm(z)

	# upper tail
	ut <- 1 - pnorm(z)

	# two tail
	tt <- pnorm(z) * 2

	alt <- match.arg(alternative)

    if (alt == "all") {
        sig = c('two-tail' = tt, 'lower-tail' = lt, 'upper-tail' = ut)
    } else if (alt == "upper-tail") {
        sig = ut
    } else if (alt == "lower-tail"){
        sig = lt
    } else {
        sig = tt
    }

    # result
    out <- list(n1 = n1, n2 = n2, phat1 = phat1, phat2 = phat2, z = z, sig = sig)

    return(out)	

}