formatter_t <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "centre")
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

formats_t <- function() {
    x <- rep("  ")
    return(x)
}


# test
ttest <- function(x, vname,
                  type = c("two-tail", "lower-tail", "upper-tail", "all"),
                  alpha = 0.05, mu = 0) {
  
  # test type
  type <- match.arg(type)

  # variable names
  var_name <- vname

  
  # computation
  n <- length(x)
  a <- alpha / 2
  df <- n - 1
  Mean <- round(mean(x), 4)
  stddev <- round(sd(x), 4)
  std_err <- round(stddev / sqrt(n), 4)
  test_stat <- round((Mean - mu) / std_err, 3)
  if (type == 'lower-tail') {
    cint <- c(-Inf, test_stat + qt(1 - alpha, df) )
  } else if (type == 'upper-tail') {
    cint <- c(test_stat - qt(1 - alpha, df), Inf)
  } else {
    cint <- qt(1 - a, df)
    cint <- test_stat + c(-cint, cint)
  }
  confint <- round(mu + cint * std_err, 4)
  # confint <- conf_int_t(Mean, stddev, n, alpha = alpha)
  mean_diff <- round((Mean - mu), 4)
  mean_diff_l <- confint[1] - mu
  mean_diff_u <- confint[2] - mu
  p_l <- pt(test_stat, df)
  p_u <- 1 - p_l
  p <- p_u * 2
  null_l <- paste("Ho: mean(", var_name, ") >=", as.character(mu))
  alt_l <- paste(" Ha: mean(", var_name, ") <", as.character(mu))
  null_u <- paste("Ho: mean(", var_name, ") <=", as.character(mu))
  alt_u <- paste("Ha: mean(", var_name, ") >", as.character(mu))
  null_t <- paste("Ho: mean(", var_name, ") =", as.character(mu))
  alt_t <- paste("Ha: mean(", var_name, ") !=", as.character(mu))
  all_l <- paste("Ha: mean <", as.character(mu))
  all_u <- paste("Ha: mean >", as.character(mu))
  all_t <- paste("Ha: mean ~=", as.character(mu))
  char_p_l <- format(p_l, digits = 0, nsmall = 4)
  char_p_u <- format(p_u, digits = 0, nsmall = 4)
  char_p <- format(p, digits = 0, nsmall = 4)
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(test_stat))
  

  # formatting output
  # compute the characters of each output and decide the overall width
  var_width <- max(nchar('Variable'), nchar(var_name))
  obs_width <- max(nchar('Obs'), nchar(n))
  mean_width <- max(nchar('Mean'), nchar(Mean))
  se_width <- max(nchar('Std. Err.'), nchar(std_err))
  sd_width <- max(nchar('Std. Dev.'), nchar(stddev))
  conf_length <- nchar(confint[1]) + nchar(confint[2]) 
  confint_length <- nchar('[95% Conf. Interval]')
  if (conf_length > confint_length) {
    conf_width <- round(conf_length / 2)
  } else {
    conf_width <- round(confint_length / 2)
  }
  t_width <- nchar(test_stat)
  df_width <- max(nchar('DF'), nchar(df))
  p_width <- max(nchar('2 Tailed'), nchar(round(p, 5)))
  md_width <- max(nchar('Difference'), nchar(mean_diff))
  md_length <- nchar(mean_diff_l) + nchar(mean_diff_u) 
  if (md_length > confint_length) {
    md_conf_width <- floor(md_length / 2)
  } else {
    md_conf_width <- floor(confint_length / 2)
  }

  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 26)
  width_2 <- sum(var_width, t_width, df_width, p_width, md_width, ceiling(md_conf_width * 2), 26)
  all_width <- round(width_1 / 3)


  # print result
  if (type == "lower-tail") {
    cat(format("One-Sample Statistics", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("Obs", obs_width), formats_t(), formatter_t("Mean", mean_width), formats_t(),  
      formatter_t("Std. Err.", se_width), formats_t(), formatter_t("Std. Dev.", sd_width), formats_t(), formatter_t("[95% Conf. Interval]", conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(n, obs_width), formats_t(), formatter_t(Mean, mean_width), formats_t(), formatter_t(stddev, sd_width),
      formats_t(), formatter_t(std_err, se_width), formats_t(), format_cil(confint[1], conf_width), format_ciu(confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_l, width = width_2, justify = "centre"))
    cat("\n", format(alt_l, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(), 
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t("[95% Conf. Interval]", md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(round(test_stat, 3), t_width), formats_t(), formatter_t(df, df_width), formats_t(), formatter_t(round(p_l, 5), p_width),
      formats_t(), formatter_t(mean_diff, md_width), formats_t(), format_cil(round(mean_diff_l,4), md_conf_width), format_ciu(round(mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
  } else if (type == "upper-tail") {
    cat(format("One-Sample Statistics", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("Obs", obs_width), formats_t(), formatter_t("Mean", mean_width), formats_t(),  
      formatter_t("Std. Err.", se_width), formats_t(), formatter_t("Std. Dev.", sd_width), formats_t(), formatter_t("[95% Conf. Interval]", conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(n, obs_width), formats_t(), formatter_t(Mean, mean_width), formats_t(), formatter_t(stddev, sd_width),
      formats_t(), formatter_t(std_err, se_width), formats_t(), format_cil(confint[1], conf_width), format_ciu(confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_u, width = width_2, justify = "centre"))
    cat("\n", format(alt_u, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(), 
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t("[95% Conf. Interval]", md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(round(test_stat, 3), t_width), formats_t(), formatter_t(df, df_width), formats_t(), formatter_t(round(p_l, 5), p_width),
      formats_t(), formatter_t(mean_diff, md_width), formats_t(), format_cil(round(mean_diff_l,4), md_conf_width), format_ciu(round(mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
  } else if (type == "two-tail") {
    cat(format("One-Sample Statistics", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("Obs", obs_width), formats_t(), formatter_t("Mean", mean_width), formats_t(),  
      formatter_t("Std. Err.", se_width), formats_t(), formatter_t("Std. Dev.", sd_width), formats_t(), formatter_t("[95% Conf. Interval]", conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(n, obs_width), formats_t(), formatter_t(Mean, mean_width), formats_t(), formatter_t(stddev, sd_width),
      formats_t(), formatter_t(std_err, se_width), formats_t(), format_cil(confint[1], conf_width), format_ciu(confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n\n", format("Two Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n", format(alt_t, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(), 
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t("[95% Conf. Interval]", md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(round(test_stat, 3), t_width), formats_t(), formatter_t(df, df_width), formats_t(), formatter_t(round(p_l, 5), p_width),
      formats_t(), formatter_t(mean_diff, md_width), formats_t(), format_cil(round(mean_diff_l,4), md_conf_width), format_ciu(round(mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
  } else {
    cat(format("One-Sample Statistics", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("Obs", obs_width), formats_t(), formatter_t("Mean", mean_width), formats_t(),  
      formatter_t("Std. Err.", se_width), formats_t(), formatter_t("Std. Dev.", sd_width), formats_t(), formatter_t("[95% Conf. Interval]", conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(var_name, var_width), formats_t(), formatter_t(n, obs_width), formats_t(), formatter_t(Mean, mean_width), formats_t(), formatter_t(stddev, sd_width),
      formats_t(), formatter_t(std_err, se_width), formats_t(), format_cil(confint[1], conf_width), format_ciu(confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n\n", format(all_l, width = all_width, justify = "centre"), format(all_t, width = all_width, justify = "centre"), format(all_u, width = all_width, justify = "centre"), "\n")
    cat(format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'))
    cat("\n", format(all_p_l, width = all_width, justify = 'centre'), format(all_p_t, width = all_width, justify = 'centre'), format(all_p_u, width = all_width, justify = 'centre'))

  }
  
}




