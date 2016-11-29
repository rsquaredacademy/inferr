# test data
data <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)
data_scaled <- scale(data)

# test statistic
test_stat <- max(abs(data - mean(data))) / sd(data)

# critical value
n <- length(data)
alpha <- 0.05
talpha <- alpha / (2 * n)
tvalue <- qt(talpha, n - 2) ^ 2

# two sided
g <- ((n - 1) / sqrt(n)) * sqrt(tvalue / (n - 2 + tvalue))

# one sided
talpha1 <- alpha / n
tvalue1 <- qt(talpha1, n - 2) ^ 2
g1 <- ((n - 1) / sqrt(n)) * sqrt(tvalue1 / (n - 2 + tvalue1)) 

grubbs_test <- function(x, alpha = 0.05, type = c("two-tail", "lower-tail", "upper-tail")) {
    
    type <- match.arg(type)
    var_name <- deparse(substitute(x))
    n <- length(x)
    df <- n - 2
    
    if (type == "lower-tail") {
        
        g <- (mean(x) - min(x)) / sd(x)
        talpha1 <- alpha / n
        tvalue1 <- qt(talpha1, n - 2) ^ 2
        g1 <- ((n - 1) / sqrt(n)) * sqrt(tvalue1 / (n - 2 + tvalue1)) 
        
    } else if (type == "upper-tail") {
        
        g <- (max(x) - mean(x)) / sd(x)
        talpha1 <- alpha / n
        tvalue1 <- qt(talpha1, n - 2) ^ 2
        g1 <- ((n - 1) / sqrt(n)) * sqrt(tvalue1 / (n - 2 + tvalue1)) 
        
    } else {
        
        g <- max(abs(data - mean(data))) / sd(data)
        
        # critical value
        talpha <- alpha / (2 * n)
        tvalue <- qt(talpha, n - 2) ^ 2
        
        # two sided
        g1 <- ((n - 1) / sqrt(n)) * sqrt(tvalue / (n - 2 + tvalue))
        
    }
    
    result <- list(G = g, Critical_Value = g1)
    return(result)
}



