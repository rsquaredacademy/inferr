# data
age <- c(65, 61, 63, 86, 70, 55, 74, 35, 72, 68, 45, 58)

# step 1: sort the data in ascending order
age_sort <- sort(age)

# length of data
n <- length(age)

# define index
index <- 1:n

# define value m
mfunc <- function(i) {
    return(qnorm((i - 0.375) / (n + 0.25)))
}
m1 <- sapply(index, mfunc)

# define u, m
u <- 1 / sqrt(n)
m <- sum(m1 ^ 2)

# define values for a
an <- (-2.706056 * (u ^ 5)) + (4.434685 * (u ^ 4)) - (2.071190 * (u ^ 3)) - 
    (0.147981 * (u ^ 2)) + (0.221157 * u) + (m1[n] / sqrt(m) )
    
an1 <- (-3.582633 * (u ^ 5)) + (5.682633 * (u ^ 4)) - (1.752461 * (u ^ 3)) - 
    (0.293762 * (u ^ 2)) + (0.042981 * u) + (m1[n - 1] / sqrt(m) )

a1 <- -an
a2 <- -an1

# epsilon
epsilon <- (m - (2 * (m1[n] ^ 2)) - (2 * (m1[n -1] ^ 2))) / 
    (1 - (2 * (an ^ 2)) - (2 * (an1 ^ 2)))

aend <- n - 2
asub <- m1[3:aend]
ainter <- asub / sqrt(epsilon)
a <- c(a1, a2, ainter, an1, an)

# statistic
w <- cor(age_sort, a) ^ 2

# mean and standard deviation
avg <- (0.0038915 * ((log(n)) ^ 3)) - (0.083751 * (log(n) ^ 2)) -
    0.31082 * log(n) - 1.5861
e <- 2.71828 
pwr <- (0.0030302 * (log(n) ^ 2)) - (0.082676 * (log(n))) - 0.4803
sdev <- e ^ pwr

# z value
z <- (log(1 - w) - avg) / sdev
pvalue <- 1 - pnorm(z)






