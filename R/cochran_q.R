data <- c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1)
dim(data) <- c(12, 4)
colnames(data) <- c('Task_1', 'Task_2', 'Task_3', 'Task_4', 'Task_5', 'Task_6')
rownames(data) <- c('User_1', 'User_1', 'User_1', 'User_1')

# computation
# row and column sums
cl <- colSums(data)
clsum <- sum(cl)
cls_sum <- sum(cl ^ 2)
g <- rowSums(data)
g
gsquare <- g ^ 2
gsum <- sum(g)
gs_sum <- sum(gsquare)
k <- ncol(data)

q <- ((k - 1) * ((k * cls_sum) - (clsum ^ 2))) / ((k * gsum) - gs_sum)
pvalue <- 1 - pchisq(q, k - 1)

da <- c(0, 0, 0, 1,
0, 0, 0, 1,
0, 0, 0, 1,
1, 1, 1, 1,
1, 0, 0, 1,
0, 1, 0, 1,
1, 0, 0, 1,
0, 0, 0, 1,
0, 1, 0, 0,
0, 0, 0, 0,
1, 0, 0, 1,
0, 0, 1, 1)

data <- matrix(da, nrow = 12, byrow = TRUE)
