# mann whitney U test
x <- c(1500, 1540, 1860, 1230, 1370, 1550, 1840, 1250, 1300, 1710)
y <- c(1340, 1300, 1620, 1070, 1210, 1170, 950, 1380, 1460, 1030)

dat <- c(x, y)
c_rank <- rank(dat)

nx <- length(x)
ny <- length(y)
n2 <- nx + 1
n <- length(dat)

x_rank <- c_rank[1:nx]
y_rank <- c_rank[n2:n]

total_1 <- sum(x_rank)
total_2 <- sum(y_rank)

ux <- (nx * ny) + ((nx * (nx + 1)) / 2) - total_1
uy <- (nx * ny) + ((ny * (ny + 1)) / 2) - total_2