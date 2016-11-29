# expected value of a random variable
exp_val <- function(x, p) {
  
  # error handling
  if (length(x) != length(p))
    stop("x and y must be of the same length.")
  if(!(is.integer(x) || is.numeric(p))) 
    stop("x must be numeric or integer.")
  if(!is.numeric(p)) 
    stop("p must be numeric.")
  if (sum(p) > 1 || sum(p) < 1)
    stop("Cumulative sum of p must equal 1.")
  
  # expected value
  return(sum(x * p))
}


# variance of a random variable
var_rv <- function(x, p) {
  
  # error handling
  if (length(x) != length(p))
    stop("x and y must be of the same length.")
  if(!(is.integer(x) || is.numeric(p))) 
    stop("x must be numeric or integer.")
  if(!is.numeric(p)) 
    stop("p must be numeric.")
  if (sum(p) > 1 || sum(p) < 1)
    stop("Cumulative sum of p must equal 1.")
  
  # expected value
  exp_rv <- exp_val(x, p)
  
  # deviance
  dev_rv <- x - exp_rv
  
  # squared deviance
  sqdev_rv <- dev_rv ^ 2
  
  return(sum(sqdev_rv * p))
  
}


# standard deviation of a random variable
stddev_rv <- function(x, p) {
  
  # error handling
  if (length(x) != length(p))
    stop("x and y must be of the same length.")
  if(!(is.integer(x) || is.numeric(p))) 
    stop("x must be numeric or integer.")
  if(!is.numeric(p)) 
    stop("p must be numeric.")
  if (sum(p) > 1 || sum(p) < 1)
    stop("Cumulative sum of p must equal 1.")
  
  # variance
  variance_rv <- var_rv(x, p)
  
  # standard deviation
  return(round(variance_rv ^ 0.5, 3))
}


# function for factorial
fact <- function(x) {
  if (x <= 1) {
    result <- 1
  } else {
    result <- x * fact(x - 1)
  }
  
  return(result)
}

# combinations
combinations <- function(n, r) {
  nr <- n - r
  n_factorial <- fact(n)
  r_factorial <- fact(r)
  nr_factorial <- fact(nr)
  
  result <- n_factorial / (r_factorial * nr_factorial)
  
  return(result)
}


# permutations
permutations <- function(n, r) {
  
  # compute the factorial
  nr <- n - r
  n_factorial <- fact(n)
  nr_factorial <- fact(nr)
  
  result <- n_factorial / nr_factorial
  
  return(result)
}


# binomial probability function
binomial_df <- function(trials, success, prob) {
  
  # compute combinations
  comb <- combinations(trials, success)
  
  # probability of failure
  prob_failure <- 1 - prob
  failure_power <- trials - success
  
  # probability of outcomes
  prob_outcome <- (prob ^ success) * (prob_failure ^ failure_power) 
  
  # compute the result
  result <- comb * prob_outcome
  
  return(result)
  
}

# expected value of binomial distribution
exp_binom <- function(n, p) {
  
  # error handling
  if (missing(n))
    stop("Number of trials must be specified.")
  if (missing(p))
    stop("Probability must be specified.")
  if (n < 0) 
    stop("Number of trials must be atleast one.")
  if (p < 0 || p > 1)
    stop("Probability must be between 0 and 1.")
  
  # expected value
  return(n * p)
}

# variance of the binomial distribution
var_binom <- function(n, p) {
  
  # error handling
  if (missing(n))
    stop("Number of trials must be specified.")
  if (missing(p))
    stop("Probability must be specified.")
  if (n < 0) 
    stop("Number of trials must be atleast one.")
  if (p < 0 || p > 1)
    stop("Probability must be between 0 and 1.")
  
  # variance
  var <- exp_binom(n, p) * (1 - p)
  return(var)
}

# standard deviation of binomial distribution
stddev_binom <- function(n, p) {
  
  # error handling
  if (missing(n))
    stop("Number of trials must be specified.")
  if (missing(p))
    stop("Probability must be specified.")
  if (n < 0) 
    stop("Number of trials must be atleast one.")
  if (p < 0 || p > 1)
    stop("Probability must be between 0 and 1.")
  
  # variance
  sd <- var_binom(n, p) ^ 0.5
  return(round(sd, 2))
  
}

# binomial distribution
binomial_dist <- function(trials, prob) {
  
  # compute probability for range of success
  range <- 0:trials
  cat("x    ", "P(X = x)\n")
  for (i in range) {
    cat(i, "    ", round(binomial_df(trials, i, prob), digits = 4), "\n")
  }
  
  # compute expected value
  exp_value <- exp_binom(trials, prob)
  variance <- var_binom(trials, prob)
  std_dev <- stddev_binom(trials, prob)
  
  # print the results on the screen
  cat("Expected Value:", round(exp_value, digits = 3), "\n")
  cat("Variance:", round(variance, digits = 3), "\n")
  cat("Standard Deviation:", round(std_dev, digits = 3), "\n")
}


# the poisson distribution function
poisson_df <- function(exp_val, occurences) {
  
  # store value of e
  e <- 2.718128
  
  # compute probability
  result <- ((exp_val ^ occurences) * (e ^ -exp_val)) / fact(occurences)
  
  return(round(result, digits = 4))
  
}




