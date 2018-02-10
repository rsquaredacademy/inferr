#' @importFrom dplyr group_by summarise_all funs mutate
#' @importFrom magrittr %>% use_series
#' @importFrom stats var sd
#' @importFrom tibble tibble as_data_frame
anova_split <- function(data, x, y, sample_mean) {
  x1 <- enquo(x)
  y1 <- enquo(y)

  by_factor <-
    data %>%
    group_by(!! y1) %>%
    select(!! y1, !! x1) %>%
    summarise_all(funs(length, mean, var, sd)) %>%
    as_data_frame() %>%
    mutate(
      sst = length * ((mean - sample_mean) ^ 2),
      sse = (length - 1) * var
    )

  return(by_factor)
}

anova_avg <- function(data, y) {
  y1 <- enquo(y)

  avg <-
    data %>%
    select(!! y1) %>%
    summarise_all(funs(mean))

  return(unlist(avg, use.names = FALSE))
}

anova_calc <- function(data, sample_stats, x, y) {
  x1 <- enquo(x)
  y1 <- enquo(y)

  var_names <-
    data %>%
    select(!! x1, !! y1) %>%
    names()

  sstr <-
    sample_stats %>%
    use_series(sst) %>%
    sum() %>%
    round(3)

  ssee <-
    sample_stats %>%
    use_series(sse) %>%
    sum() %>%
    round(3)

  total <- round(sstr + ssee, 3)
  df_sstr <- nrow(sample_stats) - 1
  df_sse <- nrow(data) - nrow(sample_stats)
  df_sst <- nrow(data) - 1
  mstr <- round(sstr / df_sstr, 3)
  mse <- round(ssee / df_sse, 3)
  f <- round(mstr / mse, 3)
  sig <- round(1 - pf(f, df_sstr, df_sse), 3)
  obs <- nrow(data)
  regs <- paste(var_names[1], "~ as.factor(", var_names[2], ")")
  model <- lm(as.formula(regs), data = data)
  reg <- summary(model)
  out <- list(
    sstr = sstr, ssee = ssee, total = total, df_sstr = df_sstr,
    df_sse = df_sse, df_sst = df_sst, mstr = mstr, mse = mse, f = f,
    sig = sig, obs = obs, model = model, reg = reg
  )
  return(out)
}

binom_comp <- function(n, success, prob) {
  n <- n
  k <- success
  obs_p <- k / n
  exp_k <- round(n * prob)
  lt <- pbinom(k, n, prob, lower.tail = T)
  ut <- pbinom(k - 1, n, prob, lower.tail = F)
  p_opp <- round(dbinom(k, n, prob), 9)
  i_p <- dbinom(exp_k, n, prob)
  i_k <- exp_k

  if (k < exp_k) {
    while (i_p > p_opp) {
      i_k <- i_k + 1
      i_p <- round(dbinom(i_k, n, prob), 9)
      if (round(i_p) == p_opp) {
        break
      }
    }

    ttf <- pbinom(k, n, prob, lower.tail = T) +
      pbinom(i_k - 1, n, prob, lower.tail = F)
  } else {
    while (p_opp <= i_p) {
      i_k <- i_k - 1
      i_p <- dbinom(i_k, n, prob)
      if (round(i_p) == p_opp) {
        break
      }
    }

    i_k <- i_k

    tt <- pbinom(i_k, n, prob, lower.tail = T) +
      pbinom(k - 1, n, prob, lower.tail = F)

    ttf <- ifelse(tt <= 1, tt, 1)
  }
  out <- list(
    n = n, k = k, exp_k = exp_k, obs_p = obs_p, exp_p = prob, ik = i_k,
    lower = round(lt, 6), upper = round(ut, 6), two_tail = round(ttf, 6)
  )
  return(out)
}

# chi square association
df_chi <- function(twoway) {
  (nrow(twoway) - 1) * (ncol(twoway) - 1)
}

efmat <- function(twoway) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
  mat2 <- matrix(colSums(twoway), nrow = 1)
  ef <- mat1 %*% mat2
  return(ef)
}

pear_chsq <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(pchisq(chi, df, lower.tail = F), 4)
  out <- list(chi = chi, sig = sig)
  return(out)
}

lr_chsq <- function(twoway, df, ef) {
  chilr <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
  sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)
  out <- list(chilr = chilr, sig_lr = sig_lr)
  return(out)
}

lr_chsq2 <- function(twoway, df, ef, ds) {
  chilr <- round(2 * sum(matrix(twoway, ncol = ds) %*% matrix(log(twoway / ef), nrow = ds)), 4)
  sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)
  out <- list(chilr = chilr, sig_lr = sig_lr)
  return(out)
}

yates_chsq <- function(twoway) {
  way2 <- twoway[, c(2, 1)]
  total <- sum(twoway)
  prods <- prod(diag(twoway)) - prod(diag(way2))
  prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
  chi_y <- round((total * (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
  sig_y <- round(pchisq(chi_y, 1, lower.tail = F), 4)
  out <- list(chi_y = chi_y, sig_y = sig_y, total = total, prod_totals = prod_totals)
  return(out)
}

mh_chsq <- function(twoway, total, prod_totals) {
  num <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
  den <- prod_totals / ((total ^ 3) - (total ^ 2))
  chimh <- round((num ^ 2) / den, 4)
  sig_mh <- round(pchisq(chimh, 1, lower.tail = F), 4)
  out <- list(chimh = chimh, sig_mh = sig_mh)
  return(out)
}

efm <- function(twoway, dk) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = dk[1])
  mat2 <- matrix(colSums(twoway), ncol = dk[2])
  ef <- mat1 %*% mat2
  return(ef)
}

pear_chi <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(pchisq(chi, df, lower.tail = F), 4)
  out <- list(chi = chi, sig = sig)
  return(out)
}

chigf <- function(x, y, chi) {
  twoway <- matrix(
    table(x, y), nrow = nlevels(as.factor(x)),
    ncol = nlevels(as.factor(y))
  )
  total <- sum(twoway)
  phi <- round(sqrt(chi / total), 4)
  cc <- round(sqrt(chi / (chi + total)), 4)
  q <- min(nrow(twoway), ncol(twoway))
  cv <- round(sqrt(chi / (total * (q - 1))), 4)
  out <- list(phi = phi, cc = cc, cv = cv)
  return(out)
}

# chi square goodness of fit
chi_cort <- function(x, y) {
  diff <- x - y - 0.5
  dif <- abs(x - y) - 0.5
  dif2 <- dif ^ 2
  dev <- round((diff / y) * 100, 2)
  std <- round(diff / sqrt(y), 2)
  chi <- round(sum(dif2 / y), 4)
  out <- list(dev = dev, std = std, chi = chi)
  return(out)
}

chigof <- function(x, y) {
  dif <- x - y
  dif2 <- dif ^ 2
  dev <- round((dif / y) * 100, 2)
  std <- round(dif / sqrt(y), 2)
  chi <- round(sum(dif2 / y), 4)
  out <- list(dev = dev, std = std, chi = chi)
  return(out)
}

# cochran's q test
coch_data <- function(x, ...) {
  if (is.data.frame(x)) {
    data <- x %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      `-`(1)
  } else {
    data <- cbind(x, ...) %>%
      apply(2, as.numeric) %>%
      `-`(1) %>%
      as.data.frame()
  }

  return(data)
}

#' @importFrom purrr map_df
#' @importFrom magrittr subtract
cochran_comp <- function(data) {
  n <- nrow(data)
  k <- ncol(data)
  df <- k - 1

  cs <-
    data %>%
    map_df(.f = as.numeric) %>%
    subtract(1) %>%
    sums()

  q <- coch(k, cs$cls_sum, cs$cl, cs$g, cs$gs_sum)

  pvalue <- 1 - pchisq(q, df)

  out <- list(
    n = n,
    df = df,
    q = q,
    pvalue = round(pvalue, 4)
  )

  return(out)
}


# levene test
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

lev_comp <- function(variable, group_var, trim.mean) {
  comp <- complete.cases(variable, group_var)
  n <- length(comp)
  k <- nlevels(group_var)
  cvar <- variable[comp]
  gvar <- group_var[comp]
  lens <- tapply(cvar, gvar, length)
  avgs <- tapply(cvar, gvar, mean)
  sds <- tapply(cvar, gvar, sd)

  bf <- lev_metric(cvar, gvar, mean)
  lev <- lev_metric(cvar, gvar, median)
  bft <- lev_metric(cvar, gvar, mean, trim = trim.mean)
  out <- list(
    bf = round(bf$fstat, 4),
    p_bf = round(bf$p, 4),
    lev = round(lev$fstat, 4),
    p_lev = round(lev$p, 4),
    bft = round(bft$fstat, 4),
    p_bft = round(bft$p, 4),
    avgs = round(avgs, 2),
    sds = round(sds, 2),
    avg = round(mean(cvar), 2),
    sd = round(sd(cvar), 2),
    n = n,
    levs = levels(gvar),
    n_df = (k - 1),
    d_df = (n - k),
    lens = lens
  )
  return(out)
}

# mcnemar test
mcdata <- function(x, y) {
  if (!is.matrix(x)) {
    stop("x must be either a table or a matrix")
  }

  if (is.matrix(x)) {
    if (length(x) != 4) {
      stop("x must be a 2 x 2 matrix")
    }
  }

  dat <- x
  return(dat)
}


mctestp <- function(dat) {
  retrieve <- matrix(c(1, 2, 2, 1), nrow = 2)
  p <- dat[retrieve]
  return(p)
}

tetat <- function(p) {
  out <- ((p[1] - p[2]) ^ 2) / sum(p)
  return(out)
}

mcpval <- function(test_stat, df) {
  out <- 1 - pchisq(test_stat, df)
  return(out)
}

mcpex <- function(dat) {
  out <- 2 * min(pbinom(dat[2], sum(dat[2], dat[3]), 0.5), pbinom(dat[3], sum(dat[2], dat[3]), 0.5))
  return(out)
}

mcstat <- function(p) {
  out <- ((abs(p[1] - p[2]) - 1) ^ 2) / sum(p)
  return(out)
}

mccpval <- function(cstat, df) {
  out <- 1 - pchisq(cstat, df)
  return(out)
}

mckappa <- function(dat) {
  agreement <- sum(diag(dat)) / sum(dat)
  expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  kappa <- (agreement - expected) / (1 - expected)
  return(kappa)
}

mcserr <- function(dat, kappa) {
  expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  out <- serr(dat, kappa, expected)
}

mcconf <- function(std_err, kappa) {
  alpha <- 0.05
  interval <- qnorm(1 - (alpha / 2)) * std_err
  ci_lower <- kappa - interval
  ci_upper <- kappa + interval
  out <- list(ci_lower = ci_lower, ci_upper = ci_upper)
  return(out)
}

prop_fact <- function(dat, p) {
  dat_per <- dat / sum(dat)
  row_sum <- rowSums(dat_per)
  col_sum <- colSums(dat_per)
  controls <- 1 - col_sum[2]
  cases <- 1 - row_sum[2]
  ratio <- cases / controls
  odds_ratio <- p[1] / p[2]
  out <- list(
    cases = cases, controls = controls, ratio = ratio,
    odds_ratio = odds_ratio
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
  variance <- ((a + b - c) / ((1 - expected) ^ 2)) / sum(dat)

  return(sqrt(variance))
}

mccomp <- function(dat) {
  p <- mctestp(dat)
  test_stat <- tetat(p)
  df <- nrow(dat) - 1
  pvalue <- mcpval(test_stat, df)
  exactp <- mcpex(dat)
  cstat <- mcstat(p)
  cpvalue <- mccpval(cstat, df)
  kappa <- mckappa(dat)
  std_err <- mcserr(dat, kappa)
  clu <- mcconf(std_err, kappa)
  k <- prop_fact(dat, p)

  out <- list(
    statistic = round(test_stat, 4), df = df,
    pvalue = round(pvalue, 4), exactp = round(exactp, 4),
    cstat = cstat, cpvalue = cpvalue, kappa = round(kappa, 4),
    std_err = round(std_err, 4), kappa_cil = round(clu$ci_lower, 4),
    kappa_ciu = round(clu$ci_upper, 4), cases = round(k$cases, 4),
    controls = round(k$controls, 4), ratio = round(k$ratio, 4),
    odratio = round(k$odds_ratio, 4)
  )
  return(out)
}

# one sample proportion test
prop_comp <- function(n, prob, alternative, phat) {
  n <- n
  phat <- phat
  p <- prob
  q <- 1 - p
  obs <- c(n * (1 - phat), n * phat)
  exp <- n * c(q, p)
  dif <- obs - exp
  dev <- round((dif / exp) * 100, 2)
  std <- round(dif / sqrt(exp), 2)
  num <- phat - prob
  den <- sqrt((p * q) / n)
  z <- round(num / den, 4)
  lt <- round(pnorm(z), 4)
  ut <- round(1 - pnorm(z), 4)
  tt <- round((1 - pnorm(abs(z))) * 2, 4)
  alt <- alternative

  if (alt == "all") {
    sig <- c("two-both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <- list(
    n = n, phat = phat, p = prob, z = z, sig = sig, alt = alt,
    obs = obs, exp = exp, deviation = format(dev, nsmall = 2),
    std = format(std, nsmall = 2)
  )

  return(out)
}

# one sample variance test
osvar_comp <- function(x, sd, confint) {
  n <- length(x)
  df <- n - 1
  xbar <- round(mean(x), 4)
  sigma <- round(sd(x), 4)
  se <- round(sigma / sqrt(n), 4)
  chi <- round((df * (sigma / sd) ^ 2), 4)

  p_lower <- pchisq(chi, df)
  p_upper <- pchisq(chi, df, lower.tail = F)
  if (p_lower < 0.5) {
    p_two <- pchisq(chi, df) * 2
  } else {
    p_two <- pchisq(chi, df, lower.tail = F) * 2
  }


  conf <- confint
  a <- (1 - conf) / 2
  al <- 1 - a
  tv <- df * sigma
  c_lwr <- round(tv / qchisq(al, df), 4)
  c_upr <- round(tv / qchisq(a, df), 4)

  out <- list(
    n = n, sd = sd, sigma = sigma, se = se, chi = chi, df = df,
    p_lower = p_lower, p_upper = p_upper, p_two = p_two, xbar = xbar,
    c_lwr = c_lwr, c_upr = c_upr, conf = conf
  )

  return(out)
}

# two sample variance test
var_comp <- function(variable, group_var) {
  comp <- complete.cases(variable, group_var)
  cvar <- variable[comp]
  gvar <- group_var[comp]

  d <- tibble(cvar, gvar)
  vals <- tibble_stats(d, "cvar", "gvar")
  lass <- tbl_stats(d, "cvar")

  lens <- vals[[2]] %>% map_int(1)
  vars <- vals[[4]] %>% map_dbl(1)

  f <- vars[1] / vars[2]
  n1 <- lens[1] - 1
  n2 <- lens[2] - 1
  lower <- pf(f, n1, n2)
  upper <- pf(f, n1, n2, lower.tail = FALSE)

  out <- list(
    f = round(f, 4), lower = round(lower, 4),
    upper = round(upper, 4),
    vars = round(vars, 2),
    avgs = round((vals[[3]] %>% map_dbl(1)), 2),
    sds = round((vals[[5]] %>% map_dbl(1)), 2),
    ses = round((vals[[6]] %>% map_dbl(1)), 2),
    avg = round(lass[2], 2),
    sd = round(lass[3], 2),
    se = round(lass[4], 2),
    n1 = n1,
    n2 = n2,
    lens = lens,
    len = lass[1]
  )

  return(out)
}

# two sample proportion test
prop_comp2 <- function(var1, var2, alt) {
  n1 <- length(var1)
  n2 <- length(var2)
  y1 <- table(var1)[[2]]
  y2 <- table(var2)[[2]]
  phat1 <- round(y1 / n1, 4)
  phat2 <- round(y2 / n2, 4)
  phat <- sum(y1, y2) / sum(n1, n2)

  # test statistic
  num <- (phat1 - phat2)
  den1 <- phat * (1 - phat)
  den2 <- (1 / n1) + (1 / n2)
  den <- sqrt(den1 * den2)
  z <- round(num / den, 4)

  lt <- round(pnorm(z), 4)
  ut <- round(pnorm(z, lower.tail = FALSE), 4)
  tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)



  if (alt == "all") {
    sig <- c("two-tail" = tt, "lower-tail" = lt, "upper-tail" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  # result
  out <- list(
    n1 = n1, n2 = n2, phat1 = phat1, phat2 = phat2, z = round(z, 3),
    sig = round(sig, 3)
  )

  return(out)
}

# one sample t test
ttest_comp <- function(x, mu, alpha, type) {
  n <- length(x)
  a <- (alpha / 2)
  df <- n - 1
  conf <- 1 - alpha
  Mean <- round(mean(x), 4)
  stddev <- round(sd(x), 4)
  std_err <- round(stddev / sqrt(n), 4)
  test_stat <- round((Mean - mu) / std_err, 3)

  if (type == "less") {
    cint <- c(-Inf, test_stat + qt(1 - alpha, df))
  } else if (type == "greater") {
    cint <- c(test_stat - qt(1 - alpha, df), Inf)
  } else {
    cint <- qt(1 - a, df)
    cint <- test_stat + c(-cint, cint)
  }

  confint <- round(mu + cint * std_err, 4)
  mean_diff <- round((Mean - mu), 4)
  mean_diff_l <- confint[1] - mu
  mean_diff_u <- confint[2] - mu
  p_l <- pt(test_stat, df)
  p_u <- pt(test_stat, df, lower.tail = FALSE)

  if (p_l < 0.5) {
    p <- p_l * 2
  } else {
    p <- p_u * 2
  }


  out <- list(
    mu = mu, n = n, df = df, Mean = Mean, stddev = stddev, std_err = std_err,
    test_stat = test_stat, confint = confint, mean_diff = mean_diff, mean_diff_l = mean_diff_l,
    mean_diff_u = mean_diff_u, p_l = p_l, p_u = p_u, p = p, conf = conf
  )

  return(out)
}

# paired sample t test
paired_comp <- function(x, y, confint, var_names) {
  n <- length(x)
  df <- (n - 1)
  xy <- paste(var_names[1], "-", var_names[2])
  data_prep <- paired_data(x, y)
  b <- paired_stats(data_prep, "key", "value")
  corr <- round(cor(x, y), 4)
  corsig <- cor_sig(corr, n)
  alpha <- 1 - confint
  confint1 <- conf_int_t(b[[1, 1]], b[[1, 2]], n, alpha = alpha) %>% round(2)
  confint2 <- conf_int_t(b[[2, 1]], b[[2, 2]], n, alpha = alpha) %>% round(2)
  confint3 <- conf_int_t(b[[3, 1]], b[[3, 2]], n, alpha = alpha) %>% round(2)
  t <- round(b[[3, 1]] / b[[3, 3]], 4)
  p_l <- pt(t, df)
  p_u <- pt(t, df, lower.tail = FALSE)
  p <- pt(abs(t), df, lower.tail = FALSE) * 2

  out <- list(
    Obs = n, b = b, conf_int1 = confint1, conf_int2 = confint2,
    conf_int_diff = confint3, corr = round(corr, 2), corsig = round(corsig, 2),
    tstat = t, p_lower = p_l, p_upper = p_u, p_two_tail = p, xy = xy, df = df
  )

  return(out)
}

# independent sample t test
indth <- function(data, x, y, a) {
  x1 <- enquo(x)
  y1 <- enquo(y)

  h <- data_split(data, !! x1, !! y1)
  h$df <- h$length - 1
  h$error <- round(qt(a, h$df), 3) * -1
  h$lower <- round(h$mean_t - (h$error * h$std_err), 3)
  h$upper <- round(h$mean_t + (h$error * h$std_err), 3)
  return(h)
}

indcomb <- function(data, y, a) {
  y1 <- enquo(y)

  comb <- da(data, !! y1)
  comb$df <- comb$length - 1
  comb$error <- round(qt(a, comb$df), 3) * -1
  comb$lower <- round(comb$mean_t - (comb$error * comb$std_err), 3)
  comb$upper <- round(comb$mean_t + (comb$error * comb$std_err), 3)
  names(comb) <- NULL
  return(comb)
}

indcomp <- function(grp_stat, alpha) {
  n1 <- grp_stat[1, 2]
  n2 <- grp_stat[2, 2]
  n <- n1 + n2
  means <- grp_stat[, 3]
  mean_diff <- means[1] - means[2]
  sd1 <- round(grp_stat[1, 4], 3)
  sd2 <- round(grp_stat[2, 4], 3)
  s1 <- round(grp_stat[1, 4] ^ 2, 3)
  s2 <- round(grp_stat[2, 4] ^ 2, 3)
  sd_dif <- round(sd_diff(n1, n2, s1, s2), 3)
  se_dif <- round(se_diff(n1, n2, s1, s2), 3)
  conf_diff <- round(conf_int_p(mean_diff, se_dif, alpha = alpha), 3)
  out <- list(
    n1 = n1, n2 = n2, n = n, mean_diff = mean_diff, sd1 = sd1,
    sd2 = sd2, s1 = s1, s2 = s2, sd_dif = sd_dif, se_dif = se_dif,
    conf_diff = conf_diff
  )
  return(out)
}

indsig <- function(n1, n2, s1, s2, mean_diff) {
  d_f <- as.vector(df(n1, n2, s1, s2))
  t <- round(mean_diff / (((s1 / n1) + (s2 / n2)) ^ 0.5), 4)
  sig_l <- round(pt(t, d_f), 4)
  sig_u <- round(pt(t, d_f, lower.tail = FALSE), 4)
  if (sig_l < 0.5) {
    sig <- round(pt(t, d_f) * 2, 4)
  } else {
    sig <- round(pt(t, d_f, lower.tail = FALSE) * 2, 4)
  }
  out <- list(d_f = d_f, t = t, sig_l = sig_l, sig_u = sig_u, sig = sig)
  return(out)
}

fsig <- function(s1, s2, n1, n2) {
  out <- round(min(
    pf(round(s1 / s2, 4), (n1 - 1), (n2 - 1)),
    pf(
      round(s1 / s2, 4), (n1 - 1), (n2 - 1),
      lower.tail = FALSE
    )
  ) * 2, 4)
  return(out)
}


indpool <- function(n1, n2, mean_diff, se_dif) {
  df_pooled <- (n1 + n2) - 2
  t_pooled <- round(mean_diff / se_dif, 4)
  sig_pooled_l <- round(pt(t_pooled, df_pooled), 4)
  sig_pooled_u <- round(pt(t_pooled, df_pooled, lower.tail = FALSE), 4)
  if (sig_pooled_l < 0.5) {
    sig_pooled <- round(pt(t_pooled, df_pooled) * 2, 4)
  } else {
    sig_pooled <- round(pt(t_pooled, df_pooled, lower.tail = FALSE) * 2, 4)
  }
  out <- list(
    df_pooled = df_pooled, t_pooled = t_pooled,
    sig_pooled_l = sig_pooled_l, sig_pooled_u = sig_pooled_u,
    sig_pooled = sig_pooled
  )
  return(out)
}

#' @importFrom rlang sym
tibble_stats <- function(data, x, y) {
  by_factor <- data %>%
    group_by(!! sym(y)) %>%
    select(!! sym(y), !! sym(x)) %>%
    summarise_all(funs(length, mean, var, sd)) %>%
    as_data_frame() %>%
    mutate(
      ses = sd / sqrt(length)
    )
  return(by_factor)
}

tbl_stats <- function(data, y) {
  avg <- data %>%
    select(y) %>%
    summarise_all(funs(length, mean, sd)) %>%
    as_data_frame() %>%
    mutate(
      se = sd / sqrt(length)
    )
  return(unlist(avg, use.names = FALSE))
}


fg <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

fk <- function(x, w) {
  x %>%
    format(width = w, justify = "centre", nsmall = 3)
}


fs <- function() {
  rep("  ")
}

fl <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "left")
}

fc <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formatter_t <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_cil <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_ciu <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
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

#' @importFrom tidyr gather
paired_data <- function(x, y) {
  d <- tibble(x = x, y = y) %>%
    mutate(z = x - y) %>%
    gather()
  return(d)
}

#' @importFrom dplyr select
paired_stats <- function(data, key, value) {
  d <- data %>%
    group_by(key) %>%
    select(value, key) %>%
    summarise_all(funs(length, mean, sd)) %>%
    as_data_frame() %>%
    mutate(
      se = sd / sqrt(length)
    ) %>%
    select(-(key:length)) %>%
    round(2)
  return(d)
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
  x1 <- enquo(x)
  y1 <- enquo(y)

  by_gender <-
    data %>%
    group_by(!! x1) %>%
    select(!! x1, !! y1) %>%
    summarise_all(funs(length, mean_t, sd_t, std_err)) %>%
    as.data.frame()

  return(by_gender)
}

da <- function(data, y) {
  y1 <- enquo(y)

  dat <-
    data %>%
    select(!! y1) %>%
    summarise_all(funs(length, mean_t, sd_t, std_err)) %>%
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
    format(width = w, justify = "centre")
}

fn <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formats <- function() {
  rep("    ")
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
  if (data > value) {
    return(1)
  } else if (data < value) {
    return(0)
  } else {
    return(NA)
  }
}

nruns2 <- function(data, value) {
  if (data <= value) {
    return(0)
  } else {
    return(1)
  }
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
  return(((n * (n - N)) / ((N ^ 2) * (N - 1))))
}

check_level <- function(data, x) {
  x1 <- enquo(x)

  data %>%
    pull(!! x1) %>%
    nlevels()
}

check_x <- function(data, x) {
  x1 <- enquo(x)

  data %>%
    pull(!! x1) %>%
    (is.factor) %>%
    `!`()
}
