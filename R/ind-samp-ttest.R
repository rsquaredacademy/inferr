library(dplyr)
library(lazyeval)

# generic method

two_sample_test <- function(data, x, y, 
  alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all'),
  mu = 0, confint = 0.95, ...) UseMethod('two_sample_test')

two_sample_test.default <- function(data, x , y, 
  alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all'),
  mu = 0, confint = 0.95, ...) {

  var_y <- y
  h <- data_split(data, x, y)

  alpha <- 1 - confint
  a <- alpha / 2

  # conf int
  h <- mutate(h,
          df = length - 1,
          error = round(qt(a, df), 3) * -1,
          lower = round(mean_t - (error * std_err), 3),
          upper = round(mean_t + (error * std_err), 3)
  )

  grp_stat <- h
  means <- grp_stat[, 3]
  g_stat <- as.matrix(h)
  levels <- g_stat[, 1]
  obs <- g_stat[, 2]
  mean <- g_stat[, 3]
  sd <- g_stat[, 4]
  serror <- g_stat[, 5]
  lower <- g_stat[, 8]
  upper <- g_stat[, 9]

  # combined
  comb <- da(data, y)
  comb <- mutate(comb,
          df = length - 1,
          error = round(qt(a, df), 3) * -1,
          lower = round(mean_t - (error * std_err), 3),
          upper = round(mean_t + (error * std_err), 3)
  )
  
  
  names(comb) <- NULL
  
  # compute sd of diff
  n1 <- grp_stat[1, 2]
  n2 <- grp_stat[2, 2]
  n <- n1 + n2
  mean_diff <- means[1] - means[2]
  sd1 <- round(grp_stat[1, 4], 3)
  sd2 <- round(grp_stat[2, 4], 3)
  s1 <- round(grp_stat[1, 4] ^ 2, 3)
  s2 <- round(grp_stat[2, 4] ^ 2, 3)
  sd_dif <- round(sd_diff(n1, n2, s1, s2), 3)
  se_dif <- round(se_diff(n1, n2, s1, s2), 3)
  conf_diff <- round(conf_int_p(mean_diff, se_dif, alpha = alpha), 3)

  
  # satterthwaite
  d_f <- as.vector(df(n1, n2, s1, s2))
  t <- round(mean_diff / (((s1 / n1) + (s2 / n2)) ^ 0.5), 4)
  sig_l <- round(pt(t, d_f), 4)
  sig_u <- round(1 - pt(t, d_f), 4)
  sig <- round(pt(t, d_f) * 2, 4)
  
  se <- se_sw(s1, s2, n1, n2)
  err_mar <- se * -qt(0.025, 170)
  con_lower <- mean_diff - err_mar
  con_upper <- mean_diff + err_mar
  
  
  # pooled variance method
  df_pooled <- (n1 + n2) - 2
  t_pooled <- round(mean_diff / se_dif, 4)
  sig_pooled_l <- round(pt(t_pooled, df_pooled), 4)
  sig_pooled_u <- round(1 - pt(t_pooled, df_pooled), 4)
  sig_pooled <- round(pt(t_pooled, df_pooled) * 2, 4)
  
  error_margin <- se_dif * -qt(0.025, 198)
  conf_lower <- mean_diff - error_margin
  conf_upper <- mean_diff + error_margin
  temp <- c(conf_lower, conf_upper)
  
  # f test for equality of variance
  f <- round(s1 / s2, 4)
  num_df <- n1 - 1
  den_df <- n2 - 1
  f_sig <- round((1 - pf(f, num_df, den_df)) * 2, 4)

  result <- list(
    levels = levels,
    obs = obs,
    n = n,
    mean = mean,
    sd = sd,
    se = serror,
    lower = lower,
    upper = upper,
    comba  = comb,
    mean_diff = mean_diff, 
    sd_dif = sd_dif,
    se_dif = se_dif,
    conf_diff = conf_diff,
    df_pooled = df_pooled,
    df_satterthwaite = d_f,
    t_pooled = t_pooled,
    t_satterthwaite = t,
    sig_pooled_l = sig_pooled_l,
    sig_pooled_u = sig_pooled_u,
    sig_pooled = sig_pooled,
    sig = sig,
    sig_l = sig_l,
    sig_u = sig_u,
    num_df = num_df,
    den_df = den_df,
    f = f,
    f_sig = f_sig,
    var_y = var_y,
    confint = confint,
    mu = mu,
    alternative = alternative
  )

  class(result) <- 'two_sample_test'
  return(result)

}

print.two_sample_test <- function(data, ...) {

  char_sig <- format(data$sig, digits = 0, nsmall = 4)
  char_sig_l <- format(data$sig_l, digits = 0, nsmall = 4)
  char_sig_u <- format(data$sig_u, digits = 0, nsmall = 4)
  char_sig_pooled <- format(data$sig_pooled, digits = 0, nsmall = 4)
  char_sig_pooled_l <- format(data$sig_pooled_l, digits = 0, nsmall = 4)
  char_sig_pooled_u <- format(data$sig_pooled_u, digits = 0, nsmall = 4)

  # hypothesis heading
  hyp_null <- paste0('Ho: mean( ', data$levels[1], ' ) - mean( ', data$levels[2], ' ) = diff = ', data$mu)
  hyp_lt <- paste0('Ha: diff < ', data$mu)
  hyp_2t <- paste0('Ha: diff ~= ', data$mu)
  hyp_ut <- paste0('Ha: diff > ', data$mu)
  conf <- data$confint * 100
  conf_char <- paste0('[', conf, '% Conf. Interval]')

  # all tests combines
  all_p_l <- paste("P < t =", char_sig_pooled_l)
  all_p_t <- paste("P > |t| =", char_sig_pooled)
  all_p_u <- paste("P > t =", char_sig_pooled_u)
  all_s_l <- paste("P < t =", char_sig_l)
  all_s_t <- paste("P > |t| =", char_sig)
  all_s_u <- paste("P > t =", char_sig_u)
  p_tval <- paste0(" t = ", as.character(data$t_pooled))
  s_tval <- paste0(" t = ", as.character(data$t_satterthwaite))

  # format output
  grp_w <- max(nchar(data$levels[1]), nchar(data$levels[2]), nchar('Combined'), 10)
  obs_w <- max(nchar('Obs'), nchar(data$obs[1]), nchar(data$obs[2]), nchar(data$n))
  mean_w <- max(nchar('Mean'), nchar(data$mean[1]), nchar(data$mean[2]), nchar(data$mean_diff), nchar(data$comba[2]))
  se_w <- max(nchar('Std. Err.'), nchar(data$se[1]), nchar(data$se[2]), nchar(data$comba[4]), nchar(data$se_dif))
  sd_w <- max(nchar('Std. Dev.'), nchar(data$sd[1]), nchar(data$sd[2]), nchar(data$comba[3]), nchar(data$sd_dif))
  df_w <- max(nchar('DF'), nchar(as.vector(data$df_pooled)), nchar(as.vector(data$df_satterthwaite)))
  t_w <- max(nchar('t Value'), nchar(as.vector(data$t_pooled)), nchar(as.vector(data$t_satterthwaite)))
  pt_w <- max(nchar('P > |t|'), nchar(as.vector(char_sig)), nchar(as.vector(char_sig_l)), nchar(as.vector(char_sig_u)), 
    nchar(as.vector(char_sig_pooled)), nchar(as.vector(char_sig_pooled_l)), nchar(as.vector(char_sig_u)))
  numdf_w <- max(nchar('Num DF'), nchar(as.vector(data$num_df)), nchar(as.vector(data$den_df)))
  f_w <- max(nchar('F Value'), nchar(as.vector(data$f)))
  fp_w <- max(nchar('P > F'), nchar(as.vector(data$f_sig)))
  conf_length <- nchar(data$lower[1]) + nchar(data$upper[1]) 
  if (conf_length > 20) {
    conf_width <- conf_length 
    conf_l_width <- ceiling(conf_width / 2)
    conf_u_width <- floor(conf_width / 2)
  } else {
    conf_width <- 20
    conf_l_width <- 10
    conf_u_width <- 10
  }
  w1 <- sum(grp_w, obs_w, mean_w, se_w, sd_w, conf_width, 25)
  w2 <- sum(grp_w, 13, 9, df_w, t_w, pt_w, 25)
  w3 <- sum(grp_w, 8, numdf_w, numdf_w, f_w, fp_w, 25)

  if (data$alternative == 'lower-tail') {

  cat(fw('Group Statistics', w = w1), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('Group', w = grp_w), formats_t(), fw('Obs', w = obs_w), formats_t(),
    fw('Mean', w = mean_w), formats_t(), fw('Std. Err.', w = se_w), formats_t(),
    fw('Std. Dev.', w = sd_w), formats_t(), conf_char, "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw((data$levels[1]), w = grp_w), formats_t(), fn(data$obs[1], w = obs_w), formats_t(), 
    fn(data$mean[1], w = mean_w), formats_t(), fn(data$se[1], w = se_w), formats_t(),
    fn(data$sd[1], w = sd_w), formats_t(), fn(data$lower[1], w = conf_l_width), fn(data$upper[1], w = conf_u_width), "\n")
  cat(fw((data$levels[2]), w = grp_w), formats_t(), fn(data$obs[2], w = obs_w), formats_t(), 
    fn(data$mean[2], w = mean_w), formats_t(), fn(data$se[2], w = se_w), formats_t(),
    fn(data$sd[2], w = sd_w), formats_t(), fn(data$lower[2], w = conf_l_width), fn(data$upper[2], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('combined', w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$comba[2], w = mean_w), formats_t(), fn(data$comba[4], w = se_w), formats_t(),
    fn(data$comba[3], w = sd_w), formats_t(), fn(data$comba[7], w = conf_l_width), fn(data$comba[8], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw(('diff'), w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$mean_diff, w = mean_w), formats_t(), fn(as.vector(data$se_dif), w = se_w), formats_t(),
    fn(as.vector(data$sd_dif), w = sd_w), formats_t(), fn(as.vector(data$conf_diff[1]), w = conf_l_width), 
    fn(as.vector(data$conf_diff[2]), w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n\n")
  cat(fw('Independent Samples Test', w = w2), "\n")
  cat(fw('------------------------', w = w2), "\n\n")
  cat(fw(hyp_null, w = w2), "\n")
  cat(fw(hyp_lt, w = w2), "\n\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw('Variable', w  = grp_w), formats_t(), fw('Method', w = 13), formats_t(),
    fw('Variances', w = 9), formats_t(), fw('DF', w = df_w), formats_t(),
    fw('t Value', w = t_w), formats_t(), fw('P < t', w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Pooled', w = 13), formats_t(),
    fw('Equal', w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(), 
    fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled_l, w = pt_w), "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Satterthwaite', w = 13), formats_t(),
    fw('Unequal', w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(), 
    fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig_l, w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n\n")
  cat(fw('Test for Equality of Variances', w = w3), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw("Variable", w = grp_w), formats_t(), fw('Method', w = 8), formats_t(),
    fw('Num DF', w = numdf_w), formats_t(), fw('Den DF', w = numdf_w), formats_t(),
    fw('F Value', w = f_w), formats_t(), fw('P > F', w = fp_w), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Folded F', w = 8), formats_t(),
    fn(data$num_df, w = numdf_w), formats_t(), fn(data$den_df, w = numdf_w), formats_t(),
    fn(data$f, w = f_w), formats_t(), fn(data$f_sig, w = fp_w), "\n")
  cat(rep("-", w3), sep = "")

  } else if (data$alternative == 'upper-tail') {

  cat(fw('Group Statistics', w = w1), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('Group', w = grp_w), formats_t(), fw('Obs', w = obs_w), formats_t(),
    fw('Mean', w = mean_w), formats_t(), fw('Std. Err.', w = se_w), formats_t(),
    fw('Std. Dev.', w = sd_w), formats_t(), conf_char, "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw((data$levels[1]), w = grp_w), formats_t(), fn(data$obs[1], w = obs_w), formats_t(), 
    fn(data$mean[1], w = mean_w), formats_t(), fn(data$se[1], w = se_w), formats_t(),
    fn(data$sd[1], w = sd_w), formats_t(), fn(data$lower[1], w = conf_l_width), fn(data$upper[1], w = conf_u_width), "\n")
  cat(fw((data$levels[2]), w = grp_w), formats_t(), fn(data$obs[2], w = obs_w), formats_t(), 
    fn(data$mean[2], w = mean_w), formats_t(), fn(data$se[2], w = se_w), formats_t(),
    fn(data$sd[2], w = sd_w), formats_t(), fn(data$lower[2], w = conf_l_width), fn(data$upper[2], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('combined', w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$comba[2], w = mean_w), formats_t(), fn(data$comba[4], w = se_w), formats_t(),
    fn(data$comba[3], w = sd_w), formats_t(), fn(data$comba[7], w = conf_l_width), fn(data$comba[8], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw(('diff'), w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$mean_diff, w = mean_w), formats_t(), fn(as.vector(data$se_dif), w = se_w), formats_t(),
    fn(as.vector(data$sd_dif), w = sd_w), formats_t(), fn(as.vector(data$conf_diff[1]), w = conf_l_width), 
    fn(as.vector(data$conf_diff[2]), w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n\n")
  cat(fw('Independent Samples Test', w = w2), "\n")
  cat(fw('------------------------', w = w2), "\n\n")
  cat(fw(hyp_null, w = w2), "\n")
  cat(fw(hyp_ut, w = w2), "\n\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw('Variable', w  = grp_w), formats_t(), fw('Method', w = 13), formats_t(),
    fw('Variances', w = 9), formats_t(), fw('DF', w = df_w), formats_t(),
    fw('t Value', w = t_w), formats_t(), fw('P < t', w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Pooled', w = 13), formats_t(),
    fw('Equal', w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(), 
    fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled_u, w = pt_w), "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Satterthwaite', w = 13), formats_t(),
    fw('Unequal', w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(), 
    fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig_u, w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n\n")
  cat(fw('Test for Equality of Variances', w = w3), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw("Variable", w = grp_w), formats_t(), fw('Method', w = 8), formats_t(),
    fw('Num DF', w = numdf_w), formats_t(), fw('Den DF', w = numdf_w), formats_t(),
    fw('F Value', w = f_w), formats_t(), fw('P > F', w = fp_w), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Folded F', w = 8), formats_t(),
    fn(data$num_df, w = numdf_w), formats_t(), fn(data$den_df, w = numdf_w), formats_t(),
    fn(data$f, w = f_w), formats_t(), fn(data$f_sig, w = fp_w), "\n")
  cat(rep("-", w3), sep = "")

  } else if (data$alternative == 'two-tail') {

  cat(fw('Group Statistics', w = w1), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('Group', w = grp_w), formats_t(), fw('Obs', w = obs_w), formats_t(),
    fw('Mean', w = mean_w), formats_t(), fw('Std. Err.', w = se_w), formats_t(),
    fw('Std. Dev.', w = sd_w), formats_t(), conf_char, "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw((data$levels[1]), w = grp_w), formats_t(), fn(data$obs[1], w = obs_w), formats_t(), 
    fn(data$mean[1], w = mean_w), formats_t(), fn(data$se[1], w = se_w), formats_t(),
    fn(data$sd[1], w = sd_w), formats_t(), fn(data$lower[1], w = conf_l_width), fn(data$upper[1], w = conf_u_width), "\n")
  cat(fw((data$levels[2]), w = grp_w), formats_t(), fn(data$obs[2], w = obs_w), formats_t(), 
    fn(data$mean[2], w = mean_w), formats_t(), fn(data$se[2], w = se_w), formats_t(),
    fn(data$sd[2], w = sd_w), formats_t(), fn(data$lower[2], w = conf_l_width), fn(data$upper[2], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw('combined', w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$comba[2], w = mean_w), formats_t(), fn(data$comba[4], w = se_w), formats_t(),
    fn(data$comba[3], w = sd_w), formats_t(), fn(data$comba[7], w = conf_l_width), fn(data$comba[8], w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(fw(('diff'), w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
    fn(data$mean_diff, w = mean_w), formats_t(), fn(as.vector(data$se_dif), w = se_w), formats_t(),
    fn(as.vector(data$sd_dif), w = sd_w), formats_t(), fn(as.vector(data$conf_diff[1]), w = conf_l_width), 
    fn(as.vector(data$conf_diff[2]), w = conf_u_width),  "\n")
  cat(rep("-", w1), sep = "", "\n\n")
  cat(fw('Independent Samples Test', w = w2), "\n")
  cat(fw('------------------------', w = w2), "\n\n")
  cat(fw(hyp_null, w = w2), "\n")
  cat(fw(hyp_2t, w = w2), "\n\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw('Variable', w  = grp_w), formats_t(), fw('Method', w = 13), formats_t(),
    fw('Variances', w = 9), formats_t(), fw('DF', w = df_w), formats_t(),
    fw('t Value', w = t_w), formats_t(), fw('P < t', w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Pooled', w = 13), formats_t(),
    fw('Equal', w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(), 
    fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled, w = pt_w), "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Satterthwaite', w = 13), formats_t(),
    fw('Unequal', w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(), 
    fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig, w = pt_w), "\n")
  cat(rep("-", w2), sep = "", "\n\n")
  cat(fw('Test for Equality of Variances', w = w3), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw("Variable", w = grp_w), formats_t(), fw('Method', w = 8), formats_t(),
    fw('Num DF', w = numdf_w), formats_t(), fw('Den DF', w = numdf_w), formats_t(),
    fw('F Value', w = f_w), formats_t(), fw('P > F', w = fp_w), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(fw(data$var_y, w = grp_w), formats_t(), fw('Folded F', w = 8), formats_t(),
    fn(data$num_df, w = numdf_w), formats_t(), fn(data$den_df, w = numdf_w), formats_t(),
    fn(data$f, w = f_w), formats_t(), fn(data$f_sig, w = fp_w), "\n")
  cat(rep("-", w3), sep = "")

  } else {

    cat(fw('Group Statistics', w = w1), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(fw('Group', w = grp_w), formats_t(), fw('Obs', w = obs_w), formats_t(),
      fw('Mean', w = mean_w), formats_t(), fw('Std. Err.', w = se_w), formats_t(),
      fw('Std. Dev.', w = sd_w), formats_t(), conf_char, "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(fw((data$levels[1]), w = grp_w), formats_t(), fn(data$obs[1], w = obs_w), formats_t(), 
      fn(data$mean[1], w = mean_w), formats_t(), fn(data$se[1], w = se_w), formats_t(),
      fn(data$sd[1], w = sd_w), formats_t(), fn(data$lower[1], w = conf_l_width), fn(data$upper[1], w = conf_u_width), "\n")
    cat(fw((data$levels[2]), w = grp_w), formats_t(), fn(data$obs[2], w = obs_w), formats_t(), 
      fn(data$mean[2], w = mean_w), formats_t(), fn(data$se[2], w = se_w), formats_t(),
      fn(data$sd[2], w = sd_w), formats_t(), fn(data$lower[2], w = conf_l_width), fn(data$upper[2], w = conf_u_width),  "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(fw('combined', w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
      fn(data$comba[2], w = mean_w), formats_t(), fn(data$comba[4], w = se_w), formats_t(),
      fn(data$comba[3], w = sd_w), formats_t(), fn(data$comba[7], w = conf_l_width), fn(data$comba[8], w = conf_u_width),  "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(fw(('diff'), w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(), 
      fn(data$mean_diff, w = mean_w), formats_t(), fn(as.vector(data$se_dif), w = se_w), formats_t(),
      fn(as.vector(data$sd_dif), w = sd_w), formats_t(), fn(as.vector(data$conf_diff[1]), w = conf_l_width), 
      fn(as.vector(data$conf_diff[2]), w = conf_u_width),  "\n")
    cat(rep("-", w1), sep = "", "\n\n\n")
    cat(fw('Independent Samples Test', w = 72), "\n")
    cat(fw('------------------------', w = w2), "\n\n")
    cat(format(hyp_null, width = 72, justify = 'centre'), "\n\n")
    cat(format('Ha: diff < 0', width = 24, justify = 'centre'), format('Ha: diff ~= 0', width = 24, justify = 'centre'), 
      format('Ha: diff > 0', width = 24, justify = 'centre'), "\n\n")
    cat(format('', width = 24, justify = 'centre'), format('Pooled', width = 24, justify = 'centre'), 
      format('', width = 24, justify = 'centre'), "\n")
    cat(rep("-", 72), sep = "", "\n")
    cat(format(p_tval, width = 24, justify = 'centre'), format(p_tval, width = 24, justify = 'centre'), format(p_tval, width = 24, justify = 'centre'), "\n")
    cat(format(all_p_l, width = 24, justify = 'centre'), format(all_p_t, width = 24, justify = 'centre'), format(all_p_u, width = 24, justify = 'centre'), "\n\n")
    cat(format('', width = 24, justify = 'centre'), format('Satterthwaite', width = 24, justify = 'centre'), 
      format('', width = 24, justify = 'centre'), "\n")
    cat(rep("-", 72), sep = "", "\n")
    cat(format(s_tval, width = 24, justify = 'centre'), format(s_tval, width = 24, justify = 'centre'), format(s_tval, width = 24, justify = 'centre'), "\n")
    cat(format(all_s_l, width = 24, justify = 'centre'), format(all_s_t, width = 24, justify = 'centre'), format(all_s_u, width = 24, justify = 'centre'), "\n\n\n")
    cat(fw('Test for Equality of Variances', w = w3), "\n")
    cat(rep("-", w3), sep = "", "\n")
    cat(fw("Variable", w = grp_w), formats_t(), fw('Method', w = 8), formats_t(),
      fw('Num DF', w = numdf_w), formats_t(), fw('Den DF', w = numdf_w), formats_t(),
      fw('F Value', w = f_w), formats_t(), fw('P > F', w = fp_w), "\n")
    cat(rep("-", w3), sep = "", "\n")
    cat(fw(data$var_y, w = grp_w), formats_t(), fw('Folded F', w = 8), formats_t(),
      fn(data$num_df, w = numdf_w), formats_t(), fn(data$den_df, w = numdf_w), formats_t(),
      fn(data$f, w = f_w), formats_t(), fn(data$f_sig, w = fp_w), "\n")
    cat(rep("-", w3), sep = "")

  }

  

}


  





# # data %>%
# #   select(female) %>%
# #   summarise(
# #     level = nlevels(female)
# #   )