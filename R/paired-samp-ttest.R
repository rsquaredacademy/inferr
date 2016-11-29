# helper functions
# source('helpers/helper.R')

# summary statistics
paired_ttest <- function(x, y, name1, name2,
  alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all'),
  mu = 0, confint = 0.95, ...) UseMethod('paired_ttest')

paired_ttest.default <- function(x, y, name1, name2,
  alternative = c('two-tail', 'lower-tail', 'upper-tail', 'all'),
  mu = 0, confint = 0.95) {

  # extract variable names
  # var_x <- deparse(substitute(x))
  # var_y <- deparse(substitute(y))
  # var_na <- c(var_x, var_y)
  n <- nrow(x)
  df <- (n - 1)

  var_names <- c(name1, name2)
  xy <- paste(var_names[1], '-', var_names[2])

  # data for diff between x and y
  trial <- extract(x, y)

  # mean, standard deviation and error computation
  a <- sapply(trial, stat)

  # round output to 4 decimal places
  b <- apply(a, c(1, 2), r)

  # correlation
  corr <- round(cor(x, y), 4)
  corsig <- cor_sig(corr, n)

  # confidence interval
  alpha <- 1 - confint
  confint1 <- conf_int_t(b[1, 1], b[2, 1], n, alpha = alpha)
  conf_int1 <- lapply(confint1, r)
  confint2 <- conf_int_t(b[1, 2], b[2, 2], n, alpha = alpha)
  conf_int2 <- lapply(confint2, r)
  confint3 <- conf_int_t(b[1, 3], b[2, 3], n, alpha = alpha)
  conf_int3 <- lapply(confint3, r)

  # t value and p value
  t <- round(b[1, 3] / b[3, 3], 4)
  p_l <- pt(t, df)
  p_u <- 1 - p_l
  p <- p_l * 2

  # result
  result <- list(
      Obs = n,
      b = b,
      conf_int1 = conf_int1,
      conf_int2 = conf_int2,
      conf_int_diff = conf_int3,
      corr = corr,
      corsig = corsig,
      tstat = t,
      p_lower = p_l,
      p_upper = p_u,
      p_two_tail = p,
      var_names = var_names,
      xy = xy,
      mu = mu,
      df = df,
      alternative = alternative,
      confint = confint
  )

  class(result) <- 'paired_ttest'
  return(result)
  
}

# print
print.paired_ttest <- function(data) {

  char_p_u <- format(data$p_upper, digits = 0, nsmall = 4)
  char_p_l <- format(data$p_lower, digits = 0, nsmall = 4)
  char_p <- format(data$p_two_tail, digits = 0, nsmall = 4)

  # hypothesis heading
  hyp_null <- paste0('Ho: mean(', data$var_names[1], ' - ', data$var_names[2], ') = ', data$mu)
  hyp_lt <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') < ', data$mu)
  hyp_ut <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') > ', data$mu)
  hyp_2t <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') ~= ', data$mu)
  conf <- data$confint * 100
  conf_char <- paste0('[', conf, '% Conf. Interval]')

  # all tests combines
  all_null <- paste0('Ho: mean(', data$var_names[1], ' - ', data$var_names[2], ') = mean(diff) = ', data$mu) 
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(data$tstat))

  # formatting output
  var_width1 <- max(nchar('Variables'), nchar(data$var_names[1]), nchar(data$var_names[2]), nchar('diff'))
  var_width <- max(nchar('Variables'), nchar(data$xy))
  obs_width <- max(nchar('Obs'), nchar(data$Obs))
  mean_width <- max(nchar('Mean'), nchar(format(max(data$b[1, ]), nsmall = 4)))
  se_width <- max(nchar('Std. Err.'), nchar(format(max(data$b[3, ]), nsmall = 4)))
  sd_width <- max(nchar('Std. Dev.'), nchar(format(max(data$b[2, ]), nsmall = 4)))
  corr_width <- nchar('Correlation')
  corsig_width <- nchar(data$corsig)
  t_width <- nchar(data$tstat)
  df_width <- max(nchar('DF'), nchar(data$df))
  p_width <- max(nchar('Sig.'), nchar(data$corsig))
  conf_length <- nchar(data$conf_int_diff[[1]]) + nchar(data$conf_int_diff[[2]])
  if (conf_length > 20) {
    conf_width <- conf_length 
    conf_l_width <- ceiling(conf_width / 2)
    conf_u_width <- ceiling(conf_width / 2)
  } else {
    conf_width <- 20
    conf_l_width <- 10
    conf_u_width <- 10
  }
  space1 <- nchar('                          ')
  space2 <- nchar('                     ')
  space3 <- nchar('                      ')
  width_1 <- sum(var_width1, obs_width, mean_width, se_width, sd_width, conf_width,space1)
  width_2 <- sum(var_width, obs_width, corr_width, corsig_width, space2)
  width_3 <- sum(var_width, t_width, df_width, p_width, conf_width, space3)
  
  # print output
  if (data$alternative == 'lower-tail') {
  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair("Variables", var_width1), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width), 
      formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_1), sep = "")
  cat('\n')
  cat(formatter_pair(data$var_names[1], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[1], mean_width), 
    formats_t(), formatter_pair(data$b[3], se_width), formats_t(), formatter_pair(data$b[2], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width), 
    format_ciu(data$conf_int1[[2]], conf_u_width))
  cat('\n', formatter_pair(data$var_names[2], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[4], mean_width), formats_t(), formatter_pair(data$b[6], se_width),
     formats_t(), formatter_pair(data$b[5], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width), 
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair('diff', var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[7], mean_width), formats_t(), formatter_pair(data$b[9], se_width),
     formats_t(), formatter_pair(data$b[8], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), 
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width), 
     formats_t(), formatter_pair("Sig.", corsig_width))
  cat("\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
   formats_t(), formatter_pair(data$corr, corr_width), formats_t(), formatter_pair(data$corsig, corsig_width), "\n")
  cat(rep("-", width_2), sep = "", "\n\n")
  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_lt, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width), 
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width), 
    formats_t(), formatter_pair(char_p_l, p_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else if (data$alternative == 'upper-tail') {
  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width), 
      formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair(data$var_names[1], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[1], mean_width), 
    formats_t(), formatter_pair(data$b[3], se_width), formats_t(), formatter_pair(data$b[2], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width), 
    format_ciu(data$conf_int1[[2]], conf_u_width))
  cat("\n", formatter_pair(data$var_names[2], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[4], mean_width), formats_t(), formatter_pair(data$b[6], se_width),
     formats_t(), formatter_pair(data$b[5], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width), 
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair('diff', var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[7], mean_width), formats_t(), formatter_pair(data$b[9], se_width),
     formats_t(), formatter_pair(data$b[8], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), 
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width), 
     formats_t(), formatter_pair("Sig.", corsig_width))
  cat("\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
   formats_t(), formatter_pair(data$corr, corr_width), formats_t(), formatter_pair(data$corsig, corsig_width), "\n")
  cat(rep("-", width_2), sep = "", "\n\n")
  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_ut, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width), 
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width), 
    formats_t(), formatter_pair(char_p_u, p_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else if (data$alternative == 'two-tail') {
  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width), 
      formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair(data$var_names[1], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[1], mean_width), 
    formats_t(), formatter_pair(data$b[3], se_width), formats_t(), formatter_pair(data$b[2], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width), 
    format_ciu(data$conf_int1[[2]], conf_u_width))
  cat("\n", formatter_pair(data$var_names[2], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[4], mean_width), formats_t(), formatter_pair(data$b[6], se_width),
     formats_t(), formatter_pair(data$b[5], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width), 
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair('diff', var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[7], mean_width), formats_t(), formatter_pair(data$b[9], se_width),
     formats_t(), formatter_pair(data$b[8], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), 
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width), 
     formats_t(), formatter_pair("Sig.", corsig_width))
  cat("\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
   formats_t(), formatter_pair(data$corr, corr_width), formats_t(), formatter_pair(data$corsig, corsig_width), "\n")
  cat(rep("-", width_2), sep = "", "\n\n")
  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_2t, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width), 
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width), 
    formats_t(), formatter_pair(char_p, p_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else {
  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width), 
      formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair(data$var_names[1], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[1], mean_width), 
    formats_t(), formatter_pair(data$b[3], se_width), formats_t(), formatter_pair(data$b[2], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width), 
    format_ciu(data$conf_int1[[2]], conf_u_width))
  cat("\n", formatter_pair(data$var_names[2], var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[4], mean_width), formats_t(), formatter_pair(data$b[6], se_width),
     formats_t(), formatter_pair(data$b[5], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width), 
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair('diff', var_width), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[7], mean_width), formats_t(), formatter_pair(data$b[9], se_width),
     formats_t(), formatter_pair(data$b[8], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width), 
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width), 
     formats_t(), formatter_pair("Sig.", corsig_width))
  cat("\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
   formats_t(), formatter_pair(data$corr, corr_width), formats_t(), formatter_pair(data$corsig, corsig_width), "\n")
  cat(rep("-", width_2), sep = "", "\n\n")
  cat(format(all_null, width = 72, justify = 'centre'), "\n\n")
  cat(format('Ha: mean(diff) < 0', width = 24, justify = 'centre'), format('Ha: mean(diff) ~= 0', width = 24, justify = 'centre'), 
    format('Ha: mean(diff) > 0', width = 24, justify = 'centre'), "\n")
  cat(format(all_tval, width = 24, justify = 'centre'), format(all_tval, width = 24, justify = 'centre'), format(all_tval, width = 24, justify = 'centre'), "\n")
  cat(format(all_p_l, width = 24, justify = 'centre'), format(all_p_t, width = 24, justify = 'centre'), format(all_p_u, width = 24, justify = 'centre'), "\n")


  }

}




