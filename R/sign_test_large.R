democrats <- rep(1, 72)
republicans <- rep(0, 103)
nota <- rep(NA, 25)
data <- c(democrats, republicans, nota)
dat <- sample(data, 200)

sign_test_2 <- function(data) {
    values <- as.vector(table(data))
    n <- sum(values)
    avg <- 0.5 * n
    stdev <- (0.25 * n) ^ 0.5
    ts <- (min(values) - avg) / stdev
    sig <- 2 * pnorm(ts)
    return(round(sig, 4))
}

sign_test_2(dat)
