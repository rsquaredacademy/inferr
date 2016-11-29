# mann whitney wilcoxon test
x <- c(23, 18 ,17, 25, 22, 19, 31, 26, 29, 33)
y <- c(21, 28, 32, 30, 41, 24, 35, 34, 27, 39, 36)

dat <- c(x, y)
c_rank <- rank(dat)

n1 <- length(x)
n2 <- length(y)
n <- length(dat)

x_rank <- c_rank[1:n1]
y_rank <- c_rank[n2:n]

sum_x <- sum(x_rank)
sum_y <- sum(y_rank)

            