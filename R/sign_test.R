data <- c(1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1)

library(dplyr)

binom_dist <- function(n, p) {
    dist <- c()
    k <- 0:n
    for (i in k) {
        d <- round(dbinom(i, n, p), 4)
        dist <- c(dist, d)
    }
    cum_dist <- cumsum(dist)
    rev_cum <- 1 - cum_dist
    out <- data.frame(k, dist, cum_dist, rev_cum)
    return(out)
}

sign_test <- function(data, p, alpha = 0.05) {
    a <- alpha / 2
    n <- length(data)
    sign_dist <- binom_dist(n, p)
    lower <- max(select(filter(sign_dist, cum_dist < a), k))
    upper <- min(select(filter(sign_dist, rev_cum < a), k))
    criteria <- c(lower + 1, upper)
    return(criteria)
}



