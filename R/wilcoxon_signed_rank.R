# example data
z <- c(9, 10, 8, 4, 8, 3, 0, 10, 15, 9)
y <- c(5, 9, 10, 8, 4, 8, 5, 3, 0, 10, 15, 9, 5)
median <- 5

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


    
    
    

    
