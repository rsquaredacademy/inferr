# test of proportion
prop_test <- function(data, prob = 0.5, conf.int = 0.95,
                      alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all')) {

	n <- length(data)
	n1 <- table(data)[[2]]
	phat <- n1 / n
	p <- prob
	q <- 1 - p

	num <- phat - prob
	den <- sqrt((p * q) / n)
	z <- num / den

	# lower tail 
	lt <- pnorm(z)

	# upper tail
	ut <- 1 - pnorm(z)

	# two tail
	tt <- (1 - pnorm(z)) * 2

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
	
	# confidence limits
	cf <- (1 - conf.int) / 2
	z <- qnorm(cf, lower.tail = F)
	se <- sqrt((p * (1 - p)) / n)

	# wald confidence limits
	wlower <- phat - (z * se)
	wupper <- phat + (z * se)
	
	# wald confidence limits with continuity correction
	wlowerc <- phat - (z * se) + (1 / (2 * n))
	wupperc <- phat + (z * se) + (1 / (2 * n))
	
	# agresti coull confidence limits
	n1ac <- n1 + (z / 2)
	nac <- n + (z ^ 2)
	pac <- n1ac / nac

	aclower <- pac - (z * sqrt(pac * (1 - pac) / nac))
	acupper <- pac + (z * sqrt(pac * (1 - pac) / nac))

	# jeffreys confidence limits
	b <- n1 + 0.5
	c <- n - n1 + 0.5
	jlower <- qbeta(cf, b, c)
	jupper <- qbeta(1 -cf, b, c)

	# wilson (score) confidence limits
	z2 <- z ^ 2
	pp <- phat * (1 - phat)
	n2 <- 2 * n
	n4 <- 4 * n

	lower <- (phat + (z2 / n2) - sqrt((pp + (z2 / n4)) / n)) / (1 + (z2 / n))
	upper <- (phat + (z2 / n2) + sqrt((pp + (z2 / n4)) / n)) / (1 + (z2 / n))

	# clopper pearson confidence limits
	n5 <- n - n1 + 1
	n6 <- n - n1
	n7 <- n1 + 1

	cplower <- (1 + (n5 / (n1 * qf(1 - cf, n2, (2 * n5))))) ^ -1
	cpupper <- (1 + (n6 / (n7 * qf(cf, (2 * n7), (2 * n6))))) ^ -1


    # result
    out <- list(n = n, phat = phat, p0 = prob, z = z, sig = sig)

    return(out)

}


# test of proportion calculator
prop_calc <- function(n, phat, prob = 0.5, alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all')) {

	n <- n
	phat <- phat
	p <- prob
	q <- 1 - p

	num <- phat - prob
	den <- sqrt((p * q) / n)
	z <- num / den

	# lower tail 
	lt <- pnorm(z)

	# upper tail
	ut <- 1 - pnorm(z)

	# two tail
	tt <- (1 - pnorm(z)) * 2

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
    out <- list(n = n, phat = phat, p0 = prob, z = z, sig = sig)

    return(out)

}