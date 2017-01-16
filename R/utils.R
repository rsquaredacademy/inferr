#' @importFrom dplyr group_by_ select_ summarise_each funs mutate
#' @importFrom magrittr %>%
#' @importFrom stats var sd
#' @importFrom tibble tibble as_data_frame
anova_split <- function(data, x, y, sample_mean) {
    by_factor <- data %>%
        group_by_(y) %>%
        select_(y, x) %>%
        summarise_each(funs(length, mean, var, sd)) %>%
        as_data_frame() %>%
    		mutate(
    			sst = length * ((mean - sample_mean) ^ 2),
    		  sse = (length - 1) * var
    		)
    return(by_factor)
}

anova_avg <- function(data, y) {
    avg <- data %>%
        select_(y) %>%
        summarise_each(funs(mean))
    return(unlist(avg, use.names = FALSE))
}

lev_metric <- function(cvar, gvar, loc, ...) {
  metric <- tapply(cvar, gvar, loc, ...)
       y <- abs(cvar - metric[gvar])
  result <- anova(lm(y ~ gvar))
     out <- list(
              fstat = result$`F value`[1],
                  p = result$`Pr(>F)`[1]
            )
  return(out)
}

serr <- function(dat, kappa, expected) {
   dat_per <- dat / sum(dat)
   row_sum <- rowSums(dat_per)
row_sum[3] <- sum(row_sum)
   col_sum <- colSums(dat_per)
   dat_per <- rbind(dat_per, col_sum)
   dat_per <- cbind(dat_per, row_sum)
        d1 <- dim(dat_per)
dat_per[d1[1], d1[2]] <- 1.0
  diagonal <- diag(dat_per)
         a <- diagonal[1] * (1 - (row_sum[1] + col_sum[1]) * (1 - kappa)) ^ 2 +
               diagonal[2] * (1 - (row_sum[2] + col_sum[2]) * (1 - kappa)) ^ 2

        x1 <- dat_per[lower.tri(dat_per)][1]
        x2 <- dat_per[upper.tri(dat_per)][1]
         b <- ((1 - kappa) ^ 2) * ((x1 * (row_sum[1] + col_sum[2]) ^ 2) +
              (x2 * (row_sum[2] + col_sum[1]) ^ 2))

         c <- ((kappa) - expected * (1 - kappa)) ^ 2
  variance <- ((a + b -c) / ((1 - expected) ^ 2)) / sum(dat)

    return(sqrt(variance))
}

tibble_stats <- function(data, x, y) {
    by_factor <- data %>%
        group_by_(y) %>%
        select_(y, x) %>%
        summarise_each(funs(length, mean, var, sd)) %>%
        as_data_frame() %>%
        mutate(
          ses = sd / sqrt(length)
        )
    return(by_factor)
}

tbl_stats <- function(data, y) {
    avg <- data %>%
        select_(y) %>%
        summarise_each(funs(length, mean, sd)) %>%
        as_data_frame() %>%
        mutate(
          se = sd / sqrt(length)
        )
    return(unlist(avg, use.names = FALSE))
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
  d <- tibble(x, y) %>%
    mutate(z = x - y)
  return(d)
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

std_err <- function(x) {
  se <- sd(x) / sqrt(length(x))
  return(round(se, 3))
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

formats <- function() {
    rep("    ")
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

sums <- function(data) {

	cl <- colSums(data)
	cls_sum <- sum(cl ^ 2)
	g <- rowSums(data)
	gs_sum <- sum(g ^ 2)
	result <- list(cl = cl, cls_sum = cls_sum, g = g, gs_sum = gs_sum)

}

coch <- function(k, cls_sum, cl, g, gs_sum) {

	out <- ((k - 1) * ((k * cls_sum) - (sum(cl) ^ 2))) / ((k * sum(g)) - gs_sum)
	return(out)

}

# function for binary coding
nruns <- function(data, value) {
    if (data > value)
        return(1)
    else if (data < value)
        return(0)
    else
        return(NA)
}

nruns2 <- function(data, value) {
    if (data <= value)
        return(0)
    else
        return(1)
}

# function for binary coding if split == TRUE
binner <- function(x, threshold) {
    x_bin <- sapply(x, nruns, threshold)
    t_index <- return_pos(x, threshold)
    l_t <- length(t_index)
    w <- c(0, 1)
    r_t <- sample(w, size = l_t, TRUE)
    for (i in seq_len(l_t)) {
        if (r_t[i] > 0) {
            x_bin[t_index[i]] <- 1
        } else
            x_bin[t_index[i]] <- 0
    }
    return(x_bin)
}

# function for count of runs
nsign <- function(x) {
    n <- length(x)
    count <- 1
    k <- x[1]
    j <- 2:n
    for (i in j) {
        l <- i - 1
        if (x[i] != x[l])
            count <- count + 1
    }
    return(count)
}

# expected runs
expruns <- function(n0, n1) {
    N <- n0 + n1
    return(((2 * n0 * n1) / N) + 1)
}

# standard deviation of runs
sdruns <- function(n0, n1) {
    N <- n0 + n1
    n <- 2 * n0 * n1
    return(((n * (n - N)) / ((N ^ 2) *(N - 1))))
}
