#' @importFrom dplyr group_by_ select_ summarise_each
#' @importFrom magrittr %>%
#' @importFrom stats var sd
anova_split <- function(data, x, y) {
    by_factor <- data %>%
        group_by_(y) %>%
        select_(y, x) %>%
        summarise_each(funs(length, mean, var, sd)) %>%
        as.data.frame()
    return(by_factor)
}

anova_avg <- function(data, y) {
    avg <- data %>%
        select_(y) %>%
        summarise_each(funs(mean)) %>%
        as.data.frame()
    return(avg)
}

fg <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

fs <- function() {
  rep("  ")
}

fl <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'left')
}

fc <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

formatter_t <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

format_cil <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')}

format_ciu <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

formats_t <- function() {
    rep("  ")
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

extract <- function(x, y) {
  z <- x - y
  dat <- as.data.frame(cbind(x, y, z))
  return(dat)
}

stat <- function(x) {
  n <- length(x)
  Mean <- mean(x)
  stdev <- sd(x)
  serror <- samp_err(stdev, n)
  out <- c(Mean, stdev, serror)
  return(out)
}

cor_sig <- function(corr, n) {
  t <- corr / ((1 - (corr ^ 2)) / (n - 2)) ^ 0.5
  df <- n - 2
  sig <- (1 - pt(t, df)) * 2
  return(round(sig, 4))
}

samp_err <- function(sigma, n) {
  sigma / (n ^ 0.5)
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

formatter_pair <- function(x, w) {
  x1 <- format(x, nsmall = 2)
  x2 <- as.character(x1)
  ret <- format(x2, width = w, justify = "centre")
  return(ret)
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
    select_(x, y) %>%
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

conf_int_p <- function(u, se, alpha = 0.05) {
  a <- alpha / 2
  error <- round(qnorm(a), 3) * -1
  lower <- u - (error * se)
  upper <- u + (error * se)
  result <- c(lower, upper)
  return(result)
}

fw <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

fn <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}
