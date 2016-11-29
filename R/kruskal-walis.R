# x <- c(25, 70, 60, 85, 95, 90, 80)
# y <- c(60, 20, 30, 15, 40, 35)
# z <- c(50, 70, 60, 80, 90, 70, 75)
# 
# x <- c(3, 4, 5, 9, 8, 10, 9)
# y <- c(4, 3, 7, 9, 11)
# z <- c(11, 12, 9, 10, 11)

library(dplyr)

main <- select(hsb, write, prog)
x1 <- filter(main, prog == 1)
y1 <- filter(main, prog == 2)
z1 <- filter(main, prog == 3)

x <- x1$write
y <- y1$write
z <- z1$write

dat <- c(x, y, z)
c_rank <- rank(dat)

nx <- length(x)
ny <- length(y)
nz <- length(z)
n2 <- nx + 1
n3 <- nx + ny 
n4 <- n3 + 1
n <- length(dat)

x_rank <- c_rank[1:nx]
y_rank <- c_rank[n2:n3]
z_rank <- c_rank[n4:n]


total_x <- sum(x_rank)
total_y <- sum(y_rank)
total_z <- sum(z_rank)


w <- ((12 / (n * (n + 1))) * (((total_x ^ 2) / nx) + 
                                  (((total_y ^ 2) / ny)) + 
                                  (((total_z ^ 2) / nz)))) - (3 * (n + 1))

sig <- round(1 - pchisq(w, 2), 6)

# function
thrice <- function(x) {
    (x ^ 3) - x
}

# correction factor
xyz <- c(x_rank, y_rank, z_rank)
pipa <- as.vector(table(xyz))

ns <- (n ^ 3) - n

t <- 1 - (sum(thrice(pipa)) / ns)

wadjusted <- w / t


