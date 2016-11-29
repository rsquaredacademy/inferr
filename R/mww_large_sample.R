x <- c(1095, 955, 1200, 1195, 925, 950, 805, 945, 875, 1055, 1025, 975)
y <- c(885, 850, 915, 950, 800, 750, 865, 1000, 1050, 935)


mww_large <- function(x, y) {
    dat <- c(x, y)
    c_rank <- rank(dat)
    
    nx <- length(x)
    ny <- length(y)
    n2 <- nx + 1
    n <- length(dat)
    
    x_rank <- c_rank[1:nx]
    y_rank <- c_rank[n2:n]
    
    total_1 <- sum(x_rank)
    total_2 <- sum(y_rank)
    
    avg <- 0.5 * nx  * (nx + ny + 1)
    stdev <- ((ny * avg) * (1 / 6)) ^ 0.5
    
    test_stat <- (total_1 - avg) / stdev
    sig <- 2 * (1 - pnorm(abs(test_stat)))
    
    result <- list(
        z = test_stat,
        pvalue = sig
    )
    
    return(result)
}

