# y <- sample(c(0, 1), 200, replace = TRUE)
# x <- sample(c(0, 1), 200, replace = TRUE)
# xy <- data.frame(x, y)
# piku <- arrange(xy, x, y)
# table(piku)


# create dummy data
dat <- matrix(c(15, 7, 6, 172), nrow = 2)
dat_per <- dat / sum(dat)
row_sum <- rowSums(dat_per)
col_sum <- colSums(dat_per)
dat_per <- rbind(dat_per, col_sum)
dat_per <- cbind(dat_per, row_sum)
d1 <- dim(dat_per)
dat_per[d1[1], d1[2]] <- 1.0

# test statistic
retrieve <- matrix(c(1, 2, 2, 1), nrow = 2)
p <- dat[retrieve]
test_stat <- ((p[1] - p[2]) ^ 2) / sum(p)
df <- nrow(dat) - 1
pvalue <- 1 - pchisq(test_stat, 1)

# kappa coefficeints
agreement <- sum(diag(dat)) / sum(dat)

# expected agreement
expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)

# kappa 
kappa <- (agreement - expected) / (1 - expected)

# variance
diagonal <- diag(dat_per)
a <- diagonal[1] * (1 - (row_sum[1] + col_sum[1]) * (1 - kappa)) ^ 2 +
    diagonal[2] * (1 - (row_sum[2] + col_sum[2]) * (1 - kappa)) ^ 2 

x1 <- dat_per[lower.tri(dat_per)][1]
x2 <- dat_per[upper.tri(dat_per)][1]
b <- ((1 - kappa) ^ 2) * ((x1 * (row_sum[1] + col_sum[2]) ^ 2) +
    (x2 * (row_sum[2] + col_sum[1]) ^ 2))

c <- ((kappa) - expected * (1 - kappa)) ^ 2
variance <- ((a + b -c) / ((1 - expected) ^ 2)) / sum(dat)
std_err <- sqrt(variance)

# confidence intervals
alpha <- 0.05
interval <- qnorm(1 - (alpha /2)) * std_err
ci_lower <- kappa - interval
ci_upper <- kappa + interval

# proportions
var1 <- col_sum[2]
var2 <- row_sum[2]
ratio <- var1 / var2
odds_ratio <- p[1] / p[2]




