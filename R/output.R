print_owanova <- function(data) {

	# width
	w1 <- nchar('Between Groups')
	w2 <- max(nchar('Squares'), nchar(data$between), nchar(data$within), nchar(data$total))
	w3 <- max(nchar('DF'), nchar(data$df_btw), nchar(data$df_btw), nchar(data$df_within), nchar(data$df_total))
	w4 <- max(nchar('Mean Square'), nchar(data$ms_btw), nchar(data$ms_within))
	w5 <- max(nchar('F'), nchar(data$f))
	w6 <- max(nchar('Sig.'), nchar(format(data$sig, nsmall = 4)))
	w <- sum(w1, w2, w3, w4, w5, w6, 21)
	w7 <- nchar(data$sigma)

	dc <- as.vector(data$tab[, 1])

	w8 <- max(nchar('Category'), max(nchar(dc)))
	w9 <- max(nchar('N'), max(nchar(data$tab[, 2])))
	w10 <- max(nchar('Mean'), max(nchar(format(data$tab[, 3], nsmall = 3))))
	w11 <- max(nchar('Std. Dev.'), max(nchar(data$tab[, 4])))
	wr <- sum(w8, w9, w10, w11, 13)


	p <- format(data$p, nsmall = 4)
	q <- nrow(data$tab)
	s <- length(data$tab)


	cat(fg('ANOVA', w), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fg('', w1), fs(), fg('Sum of', w2), fs(), fg('', w3), fs(), fg('', w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(fg('', w1), fs(), fg('Squares', w2), fs(), fg('DF', w3), fs(), fg('Mean Square', w4), fs(), fg('F', w5), fs(), fg('Sig.', w6), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fl('Between Groups', w1), fs(), fg(data$between, w2), fs(), fg(data$df_btw, w3), fs(), fg(data$ms_btw, w4), fs(), fg(data$f, w5), fs(), fg(p, w6), '\n')
	cat(fl('Within Groups', w1), fs(), fg(data$within, w2), fs(), fg(data$df_within, w3), fs(), fg(data$ms_within, w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(fl('Total', w1), fs(), fg(data$total, w2), fs(), fg(data$df_total, w3), fs(), fg('', w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(rep("-", w), sep = "", '\n\n')

	cat(fg('Report', wr), '\n')
	cat(rep("-", wr), sep = "", '\n')
	cat(fg('Category', w8), fs(), fg('N', w9), fs(), fg('Mean', w10), fs(), fg('Std. Dev.', w11), '\n')
	cat(rep("-", wr), sep = "", '\n')
	for (i in seq_len(q)) {
		cat(fc(data$tab[i, 1], w8), fs(), fg(data$tab[i, 2], w9), fs(), fg(data$tab[i, 3], w10), fs(), fg(data$tab[i, 4], w11), '\n')
	}
	cat(rep("-", wr), sep = "", '\n\n')

	cat(fl('Number of obs', 13), '=', fl(data$obs, w7), fs(), fl('R-squared', 13), '=', data$r2, '\n')
	cat(fl('Root MSE', 13), '=', data$sigma, fs(), fl('Adj R-squared', 13), '=', data$ar2, '\n\n')
}


print_binom <- function(data) {

    # widths
    w1 <- nchar('Group')
    w2 <- max(nchar('N'), nchar(data$n))
    w3 <- max(nchar('Obs. Prop'), nchar(data$obs_p))
    w4 <- max(nchar('Exp. Prop'), nchar(data$exp_p))
    w <- sum(w1, w2, w3, w4, 13)

    k0 <- data$n - data$k
    p0 <- 1 - data$obs_p
    e0 <- 1 - data$exp_p

    cat(format('Binomial Test', width = w, justify = 'centre'), '\n')
    cat(" ", rep("-", w), sep = "", '\n')
    cat(" ", format('Group', width = w1, justify = 'left'), fs(),
        format('N', width = w2, justify = 'centre'), fs(),
        format('Obs. Prop', width = w3, justify = 'centre'), fs(),
        format('Exp. Prop', width = w4, justify = 'centre'), '\n')
    cat(" ", rep("-", w), sep = "", '\n')
    cat(" ", format('0', width = w1, justify = 'centre'), fs(),
        format(k0, width = w2, justify = 'right'), fs(),
        format(p0, width = w3, justify = 'centre'), fs(),
        format(e0, width = w4, justify = 'centre', nsmall = 3), '\n')
    cat(" ", format('1', width = w1, justify = 'centre'), fs(),
        format(data$k, width = w2, justify = 'right'), fs(),
        format(data$obs_p, width = w3, justify = 'centre'), fs(),
        format(data$exp_p, width = w4, justify = 'centre', nsmall = 3), '\n')
    cat(" ", rep("-", w), sep = "", '\n')

    # test summary widths
    w6 <- nchar('Lower')
    w7 <- nchar(paste0('Pr(k <= ', data$ik, ' or k >= ', data$k, ')'))
    w8 <- nchar(paste0('Pr(k <= ', data$k, ' or k >= ', data$ik, ')'))
    w9 <- 8
    w10 <- sum(w6, w7, w9, 9)
    w11 <- sum(w6, w8, w9, 9)




    if (data$k < data$exp_k) {

        cat('\n\n', format('Test Summary', width = w11, justify = 'centre'), '\n')
        cat(" ", rep("-", w11), sep = "", '\n')
        cat(" ", format('Tail', width = w6, justify = 'left'), fs(), format('Prob', width = w8, justify = 'centre'), fs(),
        format('p-value', width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w11), sep = "", '\n')
        cat(" ", format('Lower', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ')'), width = w8, justify = 'left'), fs(),
        format(data$lower, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Upper', width = w6, justify = 'left'), fs(), format(paste0('Pr(k >= ', data$k, ')'), width = w8, justify = 'left'), fs(),
        format(data$upper, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Two', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ' or k >= ', data$ik, ')'), width = w8, justify = 'left'), fs(),
        format(data$two_tail, width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w11), sep = "", '\n')

    } else {

        cat('\n\n', format('Test Summary', width = w10, justify = 'centre'), '\n')
        cat(" ", rep("-", w10), sep = "", '\n')
        cat(" ", format('Tail', width = w6, justify = 'left'), fs(), format('Prob', width = w7, justify = 'centre'), fs(),
        format('p-value', width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w10), sep = "", '\n')
        cat(" ", format('Lower', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$k, ')'), width = w7, justify = 'left'), fs(),
        format(data$lower, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Upper', width = w6, justify = 'left'), fs(), format(paste0('Pr(k >= ', data$k, ')'), width = w7, justify = 'left'), fs(),
        format(data$upper, width = w9, justify = 'centre'),'\n')
        cat(" ", format('Two', width = w6, justify = 'left'), fs(), format(paste0('Pr(k <= ', data$ik, ' or k >= ', data$k, ')'), width = w7, justify = 'left'), fs(),
        format(data$two_tail, width = w9, justify = 'centre'),'\n')
        cat(" ", rep("-", w10), sep = "", '\n')

    }

}


print_ttest <- function(data) {

	null_l <- paste("Ho: mean(", data$var_name, ") >=", as.character(data$mu))
  alt_l <- paste(" Ha: mean(", data$var_name, ") <", as.character(data$mu))
  null_u <- paste("Ho: mean(", data$var_name, ") <=", as.character(data$mu))
  alt_u <- paste("Ha: mean(", data$var_name, ") >", as.character(data$mu))
  null_t <- paste("Ho: mean(", data$var_name, ") =", as.character(data$mu))
  alt_t <- paste("Ha: mean(", data$var_name, ") !=", as.character(data$mu))
  all_l <- paste("Ha: mean <", as.character(data$mu))
  all_u <- paste("Ha: mean >", as.character(data$mu))
  all_t <- paste("Ha: mean ~=", as.character(data$mu))
  char_p_l <- format(data$p_l, digits = 0, nsmall = 4)
  char_p_u <- format(data$p_u, digits = 0, nsmall = 4)
  char_p <- format(data$p, digits = 0, nsmall = 4)
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(data$test_stat))


  # formatting output
  # compute the characters of each output and decide the overall width
  var_width <- max(nchar('Variable'), nchar(data$var_name))
  obs_width <- max(nchar('Obs'), nchar(data$n))
  mean_width <- max(nchar('Mean'), nchar(data$Mean))
  se_width <- max(nchar('Std. Err.'), nchar(data$std_err))
  sd_width <- max(nchar('Std. Dev.'), nchar(data$stddev))
  conf_length <- nchar(data$confint[1]) + nchar(data$confint[2])
  conf_str <- paste0('[', data$conf * 100, '% Conf. Interval]')
  confint_length <- nchar(conf_str)
  if (conf_length > confint_length) {
    conf_width <- round(conf_length / 2)
  } else {
    conf_width <- round(confint_length / 2)
  }
  t_width <- nchar(data$test_stat)
  df_width <- max(nchar('DF'), nchar(data$df))
  p_width <- max(nchar('2 Tailed'), nchar(round(data$p, 5)))
  md_width <- max(nchar('Difference'), nchar(data$mean_diff))
  md_length <- nchar(data$mean_diff_l) + nchar(data$mean_diff_u)
  if (md_length > confint_length) {
    md_conf_width <- floor(md_length / 2)
  } else {
    md_conf_width <- floor(confint_length / 2)
  }

  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 26)
  width_2 <- sum(var_width, t_width, df_width, p_width, md_width, ceiling(md_conf_width * 2), 26)
  all_width <- round(width_1 / 3)

    cat(format("One-Sample Statistics", width = width_1, justify = "centre"),
     "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(),
      formatter_t("Obs", obs_width), formats_t(),
      formatter_t("Mean", mean_width),
      formats_t(), formatter_t("Std. Err.", se_width), formats_t(),
      formatter_t("Std. Dev.", sd_width), formats_t(),
      formatter_t(conf_str, conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(data$n, obs_width), formats_t(),
      formatter_t(data$Mean, mean_width),
      formats_t(), formatter_t(data$stddev, sd_width), formats_t(),
      formatter_t(data$std_err, se_width), formats_t(),
      format_cil(data$confint[1], conf_width),
      format_ciu(data$confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")

  # print result
  if (data$type == "less") {

    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_l, width = width_2, justify = "centre"))
    cat("\n", format(alt_l, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l,4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else if (data$type == "greater") {

    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_u, width = width_2, justify = "centre"))
    cat("\n", format(alt_u, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l,4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else if (data$type == "both") {

    cat("\n\n", format("Two Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n", format(alt_t, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l,4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else {

    cat("\n\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n\n", format(all_l, width = all_width, justify = "centre"), format(all_t, width = all_width, justify = "centre"), format(all_u, width = all_width, justify = "centre"), "\n")
    cat(format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'))
    cat("\n", format(all_p_l, width = all_width, justify = 'centre'), format(all_p_t, width = all_width, justify = 'centre'), format(all_p_u, width = all_width, justify = 'centre'))

  }

}
