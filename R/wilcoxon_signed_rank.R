x <- c(35.5,   44.5,  39.8,  33.3,  51.4,  51.3,  30.5,  48.9,   42.1,   40.3,
       46.8,   38.0,  40.1,  36.8,  39.3,  65.4,  42.6,  42.8,   59.8,   52.4,
       26.2,   60.9,  45.6,  27.1,  47.3,  36.6,  55.6,  45.1,   52.2,   43.5)
median <- 45

# to do
# 1. use critical values instead of p values
# 2. Write test interpretations logic for 
#   2.1 Two Tail
#   2.2 Lower Tail
#   2.3 Upper Tail

ws_rank <- function(x, median, zapprox = FALSE, cont_correct = FALSE,
    tie_correct = FALSE) {

    differ <- x - median
    dif <- differ[differ != 0]
    dif_abs <- abs(dif)
    dif_rank <- rank(dif_abs, ties.method = "average")
    dif_sign <- sign(dif)
    final_rank <- dif_rank * dif_sign
    rplus <- sum(final_rank[final_rank > 0])
    rminus <- abs(sum(final_rank[final_rank < 0]))
    n <- length(final_rank)

    # test statistic
    tstat <- min(rplus, rminus)
    
    # p values
    p1tail <- psignrank(tstat, n)                   # one tail
    p2tail <- psignrank(tstat, n, lower.tail = F)   # two tail

    # normal approximation

    z <- (tstat - (n * (n + 1) / 4)) / sqrt((n * (n + 1) * (2 * n + 1)) / 24)  

    # continuity correction
    zcc <- (abs(tstat - (n * (n + 1) / 4)) - 0.5) / sqrt((n * (n + 1) * (2 * n + 1)) / 24)

    # tie correction 
    k <- table(abs(final_rank))
    t <- sum(k[k > 1])
    t3 <- sum((k[k > 1]) ^ 3)
    ztc <- (tstat - (n * (n + 1) / 4)) / sqrt(((n * (n + 1) * (2 * n + 1)) / 24) - ((t3 - sum(t)) / 48))

    # continuity and tie correction
    zctc <- (abs(tstat - (n * (n + 1) / 4)) - 0.5) / sqrt(((n * (n + 1) * (2 * n + 1)) / 24) - ((t3 - sum(t)) / 48))


    result <- list(rplus = rplus, rminus = rminus, tstat = tstat, p1tail = p1tail,
        p2tail = p2tail, z = z, zcc = zcc, ztc = ztc, zctc = zctc)

    return(result)
}


# x <- c(5.0, 3.9, 5.2, 5.5, 2.8, 6.1, 6.4, 2.6, 1.7, 4.3)
ws_rank(x, median)    
    
    

    
