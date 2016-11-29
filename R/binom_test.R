# import data
data <- read.csv("data/hsb2.csv")

binom_sum_lower <- function(trials, success, prob) {
    range <- 0:success
    sum <- 0
    for (i in range) {
        p <- binomial_df(trials, i, prob)
        sum <- sum + p
    }
    return(sum)
}

binom_sum_upper <- function(trials, success, prob) {
    range <- success:trials
    sum <- 0
    for (i in range) {
        p <- binomial_df(trials, i, prob)
        sum <- sum + p
    }
    return(sum)
}

binom_test <- function(data, prob, two_tailed = TRUE, alpha = 0.05,
                       correct = FALSE, agresticoull = FALSE) {
    
    # length of data
    len <- length(data)
    values <- comp_unique(data)
    counts <- count(data, values)
    n1 <- counts[2]
    q <- 1 - prob
    phat <- n1 / len
    
    # use normal approximation if length of
    # data is > 25
    if (len <= 25) {
        
        p1 <- binom_sum_lower(len, n1, prob)
        p2 <- binom_sum_upper(len, n1, prob)
        p <- min(p1, p2)
        
    } else {
        
        avg <- len * prob
        stdev <- (len * prob * q) ^ 0.5
        z1 <- ((n1 + 0.5) - avg) / stdev
        z2 <- ((n1 - 0.5) - avg) / stdev
        p <- min(pnorm(z1), (1 - pnorm(z2)))
        
    }
    
    if (two_tailed != TRUE) {
        sig = p
    } else {
        sig = 2 * p
    }
    
    # wald asymptotic confidence interval
    stderr <- ((phat * (1 - phat)) /  len) ^ 0.5
    alpha2 <- alpha / 2
    a <- 1 - alpha2
    zvalue <- qnorm(a)
    
    # continuity correction
    if (correct == TRUE) {
        err_margin <- stderr * zvalue * (1 / (2 * len))
        ci_lower <- phat - err_margin
        ci_upper <- phat + err_margin
    } else {
        err_margin <- stderr * zvalue
        ci_lower <- phat - err_margin
        ci_upper <- phat + err_margin
    }
    
    # agresti coull confidence limits
    n1_till <- n1 + (zvalue / 2)
    n_till <- len + (zvalue ^ 2)
    p_till <- n1_till / n_till
    
    ac_error <- zvalue * (((p_till * (1 - p_till)) / len) ^ 0.5)
    ac_lower <- p_till - ac_error
    ac_upper <- p_till + ac_error
    
    
    # wilson confidence limits
    z2 <- (zvalue ^ 2) / len
    z3 <- z2 * 0.5
    z4 <- z2 * 0.25
    
    num1 <- (phat + z3)
    nq <- prob * q
    num2 <- zvalue * (((nq + z4) / len) ^ 0.5)
    den <- (1 + z2)
    
    wilson_lower <- (num1 - num2) / den
    wilson_upper <- (num1 + num2) / den
    
    # clopper pearson confidence limits
    cp1 <- n1 +1
    cp2 <- 2 * n1
    cp3 <- len - n1 + 1
    cp4 <- len - n1
    cp5 <- 2 * (len - n1 + 1)
    cp6 <- 2 * (n1 +1)
    cp7 <- 2 * (len - n1)
    
    f_lower <- qf(a, cp5, cp2)
    f_upper <- qf(a, cp6, cp7)
    
    cp_lower <- n1 / (n1 + (cp3 * f_lower))
    cp_upper <- (cp1 * f_upper) / (cp4 + (cp1 * f_upper))

    
    result <- list(len,
                   n1,
                   mean = avg,
                   sd = stdev,
                   phat,
                   zone = z1,
                   ztwo = z2,
                   pvalue = p,
                   significance = sig)
    return(result)
    
}










