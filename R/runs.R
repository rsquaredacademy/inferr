runs_test <- function(x, drop = FALSE, split = FALSE, mean = FALSE,
    threshold = NA) UseMethod("runs_test")

# default method
runs_test.default <- function(x, drop = FALSE,
                              split = FALSE, mean = FALSE,
                              threshold = NA) {
    # length of data
    n <- length(x)

    # error handling
    if (missing(x))
        stop("Argument x is missing. Please provide
             appropriate data.")

    if (!(is.numeric(x) || is.integer(x)))
        stop("x must be numeric or integer")

    if (is.na(threshold)) {
        y <- sort(unique(x))
        if (sum(y) == 1)
            stop("Use 0 as threshold if the data is coded as a binary.")
    }

#     if (drop != TRUE || drop != FALSE)
#         stop("drop must be either TRUE/FALSE.")
#
#     if (split != TRUE || split != FALSE)
#         stop("split must be either TRUE/FALSE.")
#
#     if (mean != TRUE || mean != FALSE)
#         stop("mean must be either TRUE/FALSE.")

    # compute threshold
    if (!(is.na(threshold))) {
        thresh <- threshold
    } else if (mean == TRUE) {
        thresh <- mean
    } else {
        thresh <- median(x, na.rm = TRUE)
    }

    # drop values equal to threshold if drop == TRUE
    if (drop == TRUE) {
        x <- x[x != thresh]
    }

    # binary coding the data based on the threshold
    if (split == TRUE) {
        x_binary <- binner(x, thresh)
    } else {
        x_binary <- sapply(x, nruns2, thresh)
    }

    # compute the number of runs
    n_runs <- nsign(x_binary)
    n1 <- sum(x_binary)
    n0 <- length(x_binary) - n1

    # compute expected runs and sd of runs
    exp_runs <- expruns(n0, n1)
    sd_runs <- sdruns(n0, n1)

    # compute the test statistic
    test_stat <- (n_runs - exp_runs) / (sd_runs ^ 0.5)
    sig <- 2 * (1 - pnorm(abs(test_stat), lower.tail = TRUE))

    # result
    result <- list(N = n,
                   threshold = thresh,
                   N_below = n0,
                   N_above = n1,
                   mean = exp_runs,
                   Var = sd_runs,
                   N_runs = n_runs,
                   z = test_stat,
                   p = sig)

    class(result) <- "runs_test"
    return(result)

}


# print method
print.runs_test <- function(x, ...) {
    cat("Runs Test\n",
        "Total Cases: ", x$N, "\n",
        "Test Value : ", x$threshold, "\n",
        "Cases < Test Value: ", x$N_below, "\n",
        "Cases > Test Value: ", x$N_above, "\n",
        "Number of Runs: ", x$N_runs, "\n",
        "Expected Runs: ", x$mean, "\n",
        "Variance (Runs): ", x$Var, "\n",
        "z Statistic: ", x$z, "\n",
        "p-value: ", x$p, "\n")
}
