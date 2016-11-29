x <- c(15, 18, 15, 13, 20, 27)
y <- c(14, 15, 11, 19, 18, 20)
z <- c(32, 30, 27, 38, 33, 22)

x <- hsb$read
y <- hsb$write
z <- hsb$math

# function
thrice <- function(x) {
    (x ^ 3) - x
}


dat <- data.frame(x, y, z)
ranks <- apply(dat, 1, rank)
dat_new <- cbind(dat, ranks[1, ], ranks[2, ], ranks[3,])
names(dat_new) <- c('x', 'y', 'z', 'x_rank',
'y_rank', 'z_rank')
totals <- apply(ranks, 1, sum)
total2 <- totals ^ 2

n <- nrow(dat)
k <- ncol(dat)

data_split <- dat_new[, 4:6]
data_split$ties <- ties
data <- filter(data_split, ties == 2 | ties == 1)


den <- 1 - ((sum(thrice(data$ties))) / ((n * k ^ 3) - (n * k)))

f <- (((12 / (n * k * (k + 1))) * sum(total2)) - (3 * n * (k + 1))) / den
sig <- 1 - pchisq(f, k -1)


