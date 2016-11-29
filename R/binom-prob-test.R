# helper functions
fs <- function() {
    x <- rep("  ")
    return(x)
}


# binomial probability test
# binomial probability calculator
binom_calc <- function(data, prob = 0.5) {

    n <- length(data)
    k <- table(data)[[2]]

    binom_test.default(n, k, prob, alternative)
    
}


# generic
binom_test.default <- function(n, success, prob = 0.5) UseMethod('binom_test')


# default
binom_test.default <- function(n, success, prob = 0.5) {

    # length of data
    n <- n

    # number of success
    k <- success
    
    # observed probability
    obs_p <- k / n

    # expected number of success
    exp_k <- n * prob

    # lower tail
    lt <- pbinom(k, n, prob, lower.tail = T)

    # upper tail
    ut <- pbinom(k - 1, n, prob, lower.tail = F)

    # two tail
    p_opp <- dbinom(k, n, prob)
    i_p <- dbinom(exp_k, n, prob)
    i_k <- exp_k

    if (k < exp_k) {
        
        while (i_p > p_opp) {
            i_k <- i_k + 1
            i_p <- dbinom(i_k, n, prob)
        }
        
        tt <- pbinom(k, n, prob, lower.tail = T) +
            pbinom(i_k - 1, n, prob, lower.tail = F)
        
    } else {
        
        while (i_p > p_opp) {
            i_k <- i_k - 1
            i_p <- dbinom(i_k, n, prob)
        }
        
        tt <- pbinom(i_k, n, prob, lower.tail = T) +
            pbinom(k - 1, n, prob, lower.tail = F)
        
    }

    # result
    out <- list(n = n, k = k, exp_k = exp_k, obs_p = obs_p, exp_p = prob, ik = i_k,
        lower = round(lt, 6), upper = round(ut, 6), two_tail = round(tt, 6))

    class(out) <- 'binom_test'

    return(out)


}


# print method
print.binom_test <- function(data, ...) {

    # widths
    w1 <- nchar('Group')
    w2 <- max(nchar('N'), nchar(data$n))
    w3 <- max(nchar('Obs. Prop'), nchar(data$obs_p))
    w4 <- max(nchar('Exp. Prop'), nchar(data$exp_p))
    w <- sum(w1, w2, w3, w4, 13)

    k0 <- data$n - data$k
    p0 <- 1 - data$obs_p
    e0 <- 1 - data$exp_p

    cat(format('Binomial Test', width = w, justify = 'centre'), '\n')
    cat(" ", rep("-", w), sep = "", '\n')
    cat(" ", format('Group', width = w1, justify = 'left'), fs(),
        format('N', width = w2, justify = 'centre'), fs(),
        format('Obs. Prop', width = w3, justify = 'centre'), fs(),
        format('Exp. Prop', width = w4, justify = 'centre'), '\n')
    cat(" ", rep("-", w), sep = "", '\n')
    cat(" ", format('0', width = w1, justify = 'centre'), fs(),
        format(k0, width = w2, justify = 'right'), fs(),
        format(p0, width = w3, justify = 'centre'), fs(),
        format(e0, width = w4, justify = 'centre', nsmall = 3), '\n')
    cat(" ", format('1', width = w1, justify = 'centre'), fs(),
        format(data$k, width = w2, justify = 'right'), fs(),
        format(data$obs_p, width = w3, justify = 'centre'), fs(),
        format(data$exp_p, width = w4, justify = 'centre', nsmall = 3), '\n')
    cat(" ", rep("-", w), sep = "", '\n')

    # test summary widths
    w6 <- nchar('Lower')
    w7 <- nchar(paste0('Pr(k <= ', data$ik, ' or k >= ', data$k))
    w8 <- nchar(paste0('Pr(k <= ', data$k, ' or k >= ', data$ik))
    w9 <- 8
    w10 <- sum(w6, w7, w9, 9)
    w11 <- sum(w6, w8, w9, 9)


    

    if (data$k < data$exp_k) {

        cat('\n\n', format('Test Summary', width = w11, justify = 'centre'), '\n')
        cat(" ", rep("-", w11), sep = "", '\n')
        cat(" ", format('Tail', width = w6, justify = 'left'), fs(), format('Prob', width = w8, justify = 'centre'), fs(),
        format('p-value', width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w11), sep = "", '\n')
        cat(" ", format('Lower', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ')'), width = w8, justify = 'left'), fs(),
        format(data$lower, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Upper', width = w6, justify = 'left'), fs(), format(paste0('Pr(k >= ', data$k, ')'), width = w8, justify = 'left'), fs(),
        format(data$upper, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Two', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ' or k >= ', data$ik), width = w8, justify = 'left'), fs(),
        format(data$two_tail, width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w11), sep = "", '\n')

    } else {

        cat('\n\n', format('Test Summary', width = w10, justify = 'centre'), '\n')
        cat(" ", rep("-", w10), sep = "", '\n')
        cat(" ", format('Tail', width = w6, justify = 'left'), fs(), format('Prob', width = w7, justify = 'centre'), fs(),
        format('p-value', width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w10), sep = "", '\n')
        cat(" ", format('Lower', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ')'), width = w7, justify = 'left'), fs(),
        format(data$lower, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Upper', width = w6, justify = 'left'), fs(), format(paste0('Pr(k >= ', data$k, ')'), width = w7, justify = 'left'), fs(),
        format(data$upper, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Two', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$ik, ' or k >= ', data$k), width = w7, justify = 'left'), fs(),
        format(data$two_tail, width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w10), sep = "", '\n')

    }

}






