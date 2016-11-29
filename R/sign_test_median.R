data <- sample(c(200:260), 62, TRUE)

flag <- function(x, y) {
    if (x > y) {
        return(1)
    } else if (x < y) {
        return(0)
    } else {
        return(NA)
    }
}

sign_test_m <- function(data, m) {
    data_binary <- sapply(data, flag, m)
    n <- length(data_binary)
    plus <- sum(data_binary, na.rm = TRUE)
    avg <- 0.5 * n
    stdev <- (0.25 * n) ^ 0.5
    test_stat <- (plus - avg) / stdev
    sig <- 2 * (1 - pnorm(abs(test_stat)))
    return(test_stat)
}


