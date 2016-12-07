# converts argument to type character and
# formats it by adding a width of 12 and
# justification of centre
formatter_freq <- function(x) {
    if (missing(x))
        stop("argument x is missing.")
    x <- as.character(x)
    return(format(x, width = 13, justify = "centre"))
}

# format output
formatter <- function(x) {
    x <- as.character(x)
    ret <- format(x, width = 13, justify = "right")
    return(ret)
}


# percent and cumulative percent computation
percent <- function(x, y) {
    out <- round((x / y) * 100, 2)
    return(format(out, nsmall = 2))
}

formata <- function(x, round, width, justify = "centre") {
    x <- round(x, round)
    x <- as.character(x)
    return(format(x, width = width, justify = justify))
}

formatas <- function(x, round, width, justify = "centre") {
    return(format(x, width = width, justify = justify))
}



# compute bin size
bin_size <- function(data, bins) {
    return((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / bins)
}


# intervals
intervals <- function(data, bins, na.rm = TRUE) {
    binsize <- bin_size(data, bins)
    bin <- bins - 1
    interval <- min(data)
    for (i in seq_len(bin)) {
        out <- interval[i] + binsize
        interval <- c(interval, out)
    }
    interval <- c(interval, max(data))
    return(interval)
}

# frequency of binned continuous variable
freq <- function(data, bins, inta) {
    result <- c()
    for (i in seq_len(bins)) {
        k <- i + 1
        freq <- data >= inta[i] & data <= inta[k]
        out <- length(data[freq])
        result <- c(result, out)
    }
    return(result)
}

# geometric mean
geometric_mean <- function(x) {
    result <- (prod(x)) ^ (1 / length(x))
    return(result)
}

# helper function
divide_by <- function(x) {
    1 / x
}

# harmonic mean
harmonic_mean <- function(x) {
    result <- length(x) / sum(sapply(x, divide_by))
    return(result)
}


# mode
summary_mode <- function(x) {
    y <- as.data.frame(table(x))
    y <- y[order(-y$Freq), ]
    mode <- y$x[which(y$Freq == max(y$Freq))]
    mode <- as.numeric(as.vector(mode))
    mode <- min(mode)
    return(mode)
}


# range
summary_range <- function(data) {
    result <- diff(range(data))
    return(result)
}

# import data
# hsb <- read.csv("data/hsb2.csv")

# helper functions
standardize <- function(x, avg, stdev, p) {
    result <- ((x - avg) / stdev) ^ p
    return(result)
}

sums <- function(x, q) {
    avg <- mean(x)
    stdev <- sd(x)
    result <- sum(sapply(x, standardize, avg, stdev, q))
    return(result)
}

# kurtosis
kurtosis <- function(x) {
    n <- length(x)
    summation <- sums(x, 4)
    part1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
    part2 <- (3 * (n - 1) ^ 2) / ((n - 2) * (n -3))
    result <- (part1 * summation) - part2
    return(result)
}

# skewness
skewness <- function(x) {
    n <- length(x)
    summation <- sums(x, 3)
    result <- (n / ((n -1) * (n -2))) * summation
    return(result)
}

# mean deviation
md_helper <- function(x, y) {
    result <- abs(x - y)
    return(result)
}

md <- function(x) {
    m <- mean(x)
    result <- sum(sapply(x, md_helper, m)) / length(x)
    return(result)
}


# coefficient of variation
summary_coefvar <- function(x) {
    result <- (sd(x) / mean(x)) * 100
    return(result)
}

# standard error of mean
std_error <- function(x) {
    result <- sd(x) / (length(x) ^ 0.5)
    return(result)
}


# sum of squares
# helper function
ss <- function(x) {
    return(x ^ 2)
}

summary_uncorrectss <- function(x) {
    result <- sum(x ^ 2)
    return(result)
}

uss <- function(x, y) {
    return((x - y) ^ 2)
}

summary_correctss <- function(x) {
    result <- sum(sapply(x, uss, mean(x)))
    return(round(result))
}


# inter-quartile range
summary_iqr <- function(x) {
    iqr <- quantile(x, 0.75) - quantile(x, 0.25)
    return(iqr)
}

# extreme observation
summary_lowobs <- function(data, lower) {
    data <- sort(data)
    lowest <- data[1:lower]
    return(lowest)
}

summary_highobs <- function(data, higher) {
    data <- sort(data, decreasing = TRUE)
    highest <- data[1:higher]
    return(highest)
}


# round up decimals to 3 digits
rounda <- function(x) {
    return(round(x, 2))
}

# format output
formatl <- function(x) {
    x <- format(x, nsmall = 2)
    ret <- format(x, width = 20, justify = "left")
    return(ret)
}

formatol <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}

fl <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "left")
    return(ret)
}

fc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}


formatr <- function(x, w) {
    x <- rounda(x)
    x <- format(x, nsmall = 2)
    ret <- format(x, width = w, justify = "right")
    return(ret)
}

formatrc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "right")
    return(ret)
}



formatc <- function(x, w) {
    if (is.numeric(x)) {
        x <- round(x, 2)
        y <- as.character(x)
        ret <- format(y, width = w, justify = "centre")
    } else {
        y <- as.character(x)
        ret <- format(y, width = w, justify = "centre")
    }
    return(ret)
}


formatnc <- function(x, w) {
    x <- rounda(x)
    x <- format(x, nsmall = 2)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}


formats <- function() {
    x <- rep("    ")
    return(x)
}

format_gap <- function(w) {
    x <- rep("", w)
    return(x)
}

return_pos <- function(data, number) {
    out <- c()
    for (i in seq_len(length(data))) {
        if (data[i] == number) {
            out <- c(out, i)
        }
    }
    return(out)
}

returnas <- function(data, unike) {
    out <- c()
    for (i in seq_along(unike)) {
        k <- return_pos(data, unike[i])
        out <- c(out, k)
    }
    return(unique(out))
}

# row percent
row_pct <- function(mat, tot) {
    d <- dim(mat)
    rows <- d[1]
    l <- length(tot)
    result <- c()
    for (i in seq_len(rows)) {
        diva <- mat[i, ] / tot[i]
        result <- rbind(result, diva)
    }
    return(result)
}


# column percent
col_pct <- function(mat, tot) {
    d <- dim(mat)
    cols <- d[2]
    l <- length(tot)
    result <- c()
    for (i in seq_len(cols)) {
        diva <- mat[, i] / tot[i]
        result <- cbind(result, diva)
    }
    return(result)
}


# round up decimals to 3 digits
rounda <- function(x) {
    round(x, 2)
}

l <- function(x) {
    x <- as.character(x)
    k <- grep("\\$", x)
    if (length(k) == 1) {
        temp <- strsplit(x, "\\$")
        out <- temp[[1]][2]
    } else {
        out <- x
    }
    return(out)
}


# paired t test
extract <- function(x, y) {
  z <- x - y
  dat <- as.data.frame(cbind(x, y, z))
  return(dat)
}

samp_err <- function(sigma, n) {
  result <- sigma / (n ^ 0.5)
  return(result)
}

# case 2: sigma unknown
conf_int_t <- function(u, s, n, alpha = 0.05) {
  a <- alpha / 2
  df <- n - 1
  error <- round(qt(a, df), 3) * -1
  lower <- u - (error * samp_err(s, n))
  upper <- u + (error * samp_err(s, n))
  result <- c(lower, upper)
  return(result)
}

stat <- function(x) {
  n <- length(x)
  Mean <- mean(x)
  stdev <- sd(x)
  serror <- samp_err(stdev, n)
  out <- c(Mean, stdev, serror)
  # out <- list(mean = Mean, sd = stdev)
  return(out)
}

cor_sig <- function(corr, n) {
  t <- corr / ((1 - (corr ^ 2)) / (n - 2)) ^ 0.5
  df <- n - 2
  sig <- (1 - pt(t, df)) * 2
  return(round(sig, 4))
}

formatter_pair <- function(x, w) {
  x1 <- format(x, nsmall = 4)
  x2 <- as.character(x1)
  ret <- format(x2, width = w, justify = "centre")
  return(ret)
}


format_cor <- function(x) {
  x1 <- format(x, nsmall = 4)
  x2 <- as.character(x1)
  ret <- format(x2, width = 13, justify = "centre")
  return(ret)
}

format_cil <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

format_ciu <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

r <- function(x) {
  return(round(x, 4))
}

fs <- function() {
    x <- rep("  ")
    return(x)
}

fg <- function(x, w) {
    z <- as.character(x)
    y <- format(z, width = w, justify = 'centre')
    return(y)
}

# basic computations
anova_split <- function(data, x, y) {
    by_factor <- data %>%
        group_by_(y) %>%
        select_(x) %>%
        summarise_each(funs(length, mean, var, sd)) %>%
        as.data.frame()
    return(by_factor)
}

overall_stats <- function(data, x) {
    by_stats <- data %>%
        select_(x) %>%
        summarise_each(funs(length, mean, sd)) %>%
        as.data.frame()
    return(by_stats)
}

anova_avg <- function(data, y) {
    avg <- data %>%
        select_(y) %>%
        summarise_each(funs(mean)) %>%
        as.data.frame()
    return(avg)
}

conf_int_t <- function(u, s, n, alpha = 0.05) {
  a <- alpha / 2
  df <- n - 1
  error <- round(qt(a, df), 3) * -1
  lower <- u - (error * samp_err(s, n))
  upper <- u + (error * samp_err(s, n))
  result <- c(lower, upper)
  return(result)
}

conf_int_p <- function(u, se, alpha = 0.05) {
  a <- alpha / 2
  error <- round(qnorm(a), 3) * -1
  lower <- u - (error * se)
  upper <- u + (error * se)
  result <- c(lower, upper)
  return(result)
}

std_err <- function(x) {
  se <- sd(x) / sqrt(length(x))
  return(round(se, 3))
}

mean_t <- function(x) {
    return(round(mean(x), 3))
}

sd_t <- function(x) {
    s <- sd(x)
    return(round(s, 3))
}

data_split <- function(data, x, y) {
  by_gender <- data %>%
    group_by_(x) %>%
    select_(y) %>%
    summarise_each(funs(length, mean_t, sd_t, std_err)) %>%
    as.data.frame()
  return(by_gender)
}

da <- function(data, y) {
  dat <- data %>%
    select_(y) %>%
    summarise_each(funs(length, mean_t, sd_t, std_err)) %>%
    as.data.frame()
  return(dat)
}

sd_diff <- function(n1, n2, s1, s2) {
  n1 <- n1 - 1
  n2 <- n2 - 1
  n <- (n1 + n2) - 2
  return(((n1 * s1 + n2 * s2) / n) ^ 0.5)
}

se_diff <- function(n1, n2, s1, s2) {
    df <- n1 + n2 - 2
    n_1 <- n1 - 1
    n_2 <- n2 - 1
    v <- (n_1 * s1 + n_2 * s2) / df
    return(sqrt(v * (1 / n1 + 1 / n2)))
}

se_sw <- function(s1, s2, n1, n2) {
  return(((s1 / n1) + (s2 / n2)) ^ 0.5)
}


df <- function(n1, n2, s1, s2) {
  sn1 <- s1 / n1
  sn2 <- s2 / n2
  m1 <- 1 / (n1 - 1)
  m2 <- 1 / (n2 - 1)
  num <- (sn1 + sn2) ^ 2
  den <- (m1 * (sn1 ^ 2)) + (m2 * (sn2 ^ 2))
  return(round(num / den))
}


conf_int_pooled <- function(mean_diff, se_diff, alpha = 0.05) {
  a <- alpha / 2
  t <- qt(0.025, 198)
}

# format functions
fw <- function(x, w) {
    x <- as.character(x)
    z <- format(x, width = w, justify = 'centre')
    return(z)
}

fn <- function(x, w) {
    y <- as.character(x)
    z <- format(y, width = w, justify = 'centre')
    return(z)
}


formats_t <- function() {
    x <- '   '
    return(x)
}

lns <- function(w) {
    cat(rep('-', w), sep = "")
}


fl <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "left")
    return(ret)
}

fc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}

# helper functions: fitted line properties
l <- function(x) {
    x <- as.character(x)
    k <- grep("\\$", x)
    if (length(k) == 1) {
        temp <- strsplit(x, "\\$")
        out <- temp[[1]][2]
    } else {
        out <- x
    }
    return(out)
}


# one-samp-var-test
formatter_t <- function(x, w) {
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

formatter_n <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}


combinations <- function(n, r) {
    factorial(n) / (factorial(n - r) * factorial(r))
}


format_cil <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "left")
  return(ret1)
}

format_ciu <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "left")
  return(ret1)
}
