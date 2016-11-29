# skewness and kurtosis
# helper functions

# standardise
stand2 <- function(x, avg, p) {
    result <- (x - avg) ^ p
    return(result)
}

sums_jarque <- function(x, q) {
    avg <- mean(x)
    result <- sum(sapply(x, stand2, avg, q))
    return(result)
}

# kurtosis
kurtosis <- function(x) {
    n <- length(x)
    summation <- sums(x, 4)
    part1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
    part2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n -3))
    result <- (part1 * summation) - part2
    return(result)
}


# skewness
sk <- function(x) {
    n <- length(x)
    num <- sums_jarque(x, 3)
    den <- (sd(x) ^ 2) ^ 1.5
    summation <- num / den
    result <- (n / ((n -1) * (n -2))) * summation
    return(result)
}


jarque_test <- function(x) {
    
    s <- sk(x)
    k <- kurtosis(x)
    n <- length(x)
    
    jb <- (n / 6) * ((s ^ 2) + ((k ^ 2) / 24))
    return(jb)
}

