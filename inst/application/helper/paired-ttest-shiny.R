#' @importFrom stats cor
#' @title Paired t test
#' @description \code{paired_ttest} tests that \code{x} and \code{y} have the
#' same mean, assuming paired data.
#' @param x a numeric vector
#' @param y a numeric vector
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter.
#' @return \code{paired_ttest} returns an object of class \code{"paired_ttest"}.
#' An object of class \code{"paired_ttest"} is a list containing the
#' following components:
#'
#' \item{Obs}{number of observations}
#' \item{b}{mean, standard deviation and standard error of \code{x}, \code{y}
#' and their difference}
#' \item{tstat}{t statistic}
#' \item{p_lower}{lower one-sided p-value}
#' \item{p_upper}{upper one-sided p-value}
#' \item{p_two_tail}{two sided p-value}
#' \item{corr}{Correlation of \code{x} and \code{y}}
#' \item{corsig}{p-value of correlation test}
#' \item{conf_int1}{confidence interval for mean of \code{x}}
#' \item{conf_int2}{confidence interval for mean of \code{y}}
#' \item{conf_int_diff}{confidence interval for mean of difference of \code{x}
#' and \code{y}}
#' \item{df}{degrees of freedom}
#' \item{confint}{confidence level}
#' \item{alternative}{alternative hypothesis}
#' \item{var_names}{names of \code{x} and \code{y}}
#' \item{xy}{string used in printing results of the test}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#' @examples
#' paired_ttest(hsb$read, hsb$write, alternative = 'less')
#' paired_ttest(hsb$read, hsb$write, alternative = 'greater')
#' paired_ttest(hsb$read, hsb$write, alternative = 'both')
#' paired_ttest(hsb$read, hsb$write, alternative = 'all')
#' @export
#'
paired_ttest_shiny <- function(data, x, y, confint = 0.95,
  alternative = c('both', 'less', 'greater', 'all')) UseMethod('paired_ttest_shiny')

#' @export
#'
paired_ttest_shiny.default <- function(data, x, y, confint = 0.95,
  alternative = c('both', 'less', 'greater', 'all')) {

  if (!is.numeric(confint)) {
    stop('confint must be numeric')
  }

     method <- match.arg(alternative)
      var_x <- x
      var_y <- y
  var_names <- c(var_x, var_y)
          x1 <- data %>% select_(x) %>% `[[`(1)
          y1 <- data %>% select_(y) %>% `[[`(1)
          n <- length(x1)
         df <- (n - 1)
         xy <- paste(var_names[1], '-', var_names[2])
  data_prep <- paired_data(x1, y1)
         b  <- paired_stats(data_prep, 'key', 'value')
       corr <- round(cor(x1, y1), 4)
     corsig <- cor_sig(corr, n)
      alpha <- 1 - confint
   confint1 <- conf_int_t(b[[1, 1]], b[[1, 2]], n, alpha = alpha) %>% round(2)
   confint2 <- conf_int_t(b[[2, 1]], b[[2, 2]], n, alpha = alpha) %>% round(2)
   confint3 <- conf_int_t(b[[3, 1]], b[[3, 2]], n, alpha = alpha) %>% round(2)
          t <- round(b[[3, 1]] / b[[3, 3]], 4)
        p_l <- pt(t, df)
        p_u <- pt(t, df, lower.tail = FALSE)
          p <- pt(abs(t), df, lower.tail = FALSE) * 2

  result <- list(
              Obs = n,
                b = b,
        conf_int1 = confint1,
        conf_int2 = confint2,
    conf_int_diff = confint3,
             corr = round(corr, 2),
           corsig = round(corsig, 2),
            tstat = t,
          p_lower = p_l,
          p_upper = p_u,
       p_two_tail = p,
        var_names = var_names,
               xy = xy,
               df = df,
      alternative = method,
          confint = confint)

  class(result) <- 'paired_ttest_shiny'
  return(result)
}

#' @export
#'
print.paired_ttest_shiny <- function(x, ...) {
  print_paired_ttest(x)
}


print_paired_ttest <- function(data) {

	char_p_u <- format(data$p_upper, digits = 0, nsmall = 3)
  char_p_l <- format(data$p_lower, digits = 0, nsmall = 3)
  char_p <- format(data$p_two_tail, digits = 0, nsmall = 3)

  # hypothesis heading
  hyp_null <- paste0('Ho: mean(', data$var_names[1], ' - ', data$var_names[2], ') = ', '0')
  hyp_lt <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') < ', '0')
  hyp_ut <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') > ', '0')
  hyp_2t <- paste0('Ha: mean(', data$var_names[1], ' - ', data$var_names[2], ') ~= ', '0')
  conf <- data$confint * 100
  conf_char <- paste0('[', conf, '% Conf. Interval]')

  # all tests combines
  all_null <- paste0('Ho: mean(', data$var_names[1], ' - ', data$var_names[2], ') = mean(diff) = ', '0')
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(data$tstat))

  # formatting output
  var_width1 <- max(nchar('Variables'), nchar(data$var_names[1]), nchar(data$var_names[2]), nchar('diff'))
  var_width <- max(nchar('Variables'), nchar(data$xy))
  obs_width <- max(nchar('Obs'), nchar(data$Obs))
  mean_width <- max(nchar('Mean'), nchar(format(max(data$b[['mean']]), nsmall = 2)))
  se_width <- max(nchar('Std. Err.'), nchar(format(max(data$b[['se']]), nsmall = 2)))
  sd_width <- max(nchar('Std. Dev.'), nchar(format(max(data$b[['sd']]), nsmall = 2)))
  corr_width <- nchar('Correlation')
  corsig_width <- max(nchar('Sig.'), nchar(data$corsig))
  t_width <- nchar(data$tstat)
  df_width <- max(nchar('DF'), nchar(data$df))
  p_width <- max(nchar('Sig.'), nchar(format(data$corsig, nsmall = 3)))
  conf_length <- max(sum(nchar(data$conf_int1)), sum(nchar(data$conf_int2)))
  if (conf_length > 20) {
    conf_width <- conf_length
    conf_l_width <- ceiling(conf_width / 2)
    conf_u_width <- ceiling(conf_width / 2)
  } else {
    conf_width <- 20
    conf_l_width <- 10
    conf_u_width <- 10
  }
  space1 <- 20
  space2 <- 13
  space3 <- 13
  width_1 <- sum(var_width1, obs_width, mean_width, se_width, sd_width, conf_width,space1)
  width_2 <- sum(var_width, obs_width, corr_width, corsig_width, space2)
  width_3 <- sum(var_width, t_width, df_width, p_width, space3)

  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "", "\n")
  cat(formatter_pair("Variables", var_width1), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width),
      formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n")
  cat(rep("-", width_1), sep = "")
  cat('\n', formatter_pair(data$var_names[1], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[1, 1]], mean_width),
    formats_t(), formatter_pair(data$b[[1, 3]], se_width), formats_t(), formatter_pair(data$b[[1,2]], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width),
    format_ciu(data$conf_int1[[2]], conf_u_width))
  cat('\n', formatter_pair(data$var_names[2], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[2, 1]], mean_width), formats_t(), formatter_pair(data$b[[2, 3]], se_width),
     formats_t(), formatter_pair(data$b[[2, 2]], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width),
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n", formatter_pair('diff', var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[3, 1]], mean_width), formats_t(), formatter_pair(data$b[[3, 3]], se_width),
     formats_t(), formatter_pair(data$b[[3, 2]], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width),
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n")
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width),
     formats_t(), formatter_pair("Sig.", corsig_width))
  cat("\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
   formats_t(), formatter_pair(data$corr, corr_width), formats_t(), format(data$corsig, corsig_width), "\n")
  cat(rep("-", width_2), sep = "", "\n\n")

  # print output
  if (data$alternative == 'less') {

  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_lt, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
    formats_t(), formatter_pair(char_p_l, p_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else if (data$alternative == 'greater') {

  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_ut, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
    formats_t(), formatter_pair(char_p_u, p_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else if (data$alternative == 'both') {

  cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
  cat(format('-------------------', width = width_3, justify = "centre"), "\n")
  cat(format(hyp_null, width = width_3, justify = 'centre'), "\n")
  cat(format(hyp_2t, width = width_3, justify = 'centre'), "\n\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
     formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n")
  cat(rep("-", width_3), sep = "")
  cat("\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
    formats_t(), formatter_pair(char_p, p_width), "\n")
  cat(rep("-", width_3), sep = "")

    } else {

  cat(format(all_null, width = 72, justify = 'centre'), "\n\n")
  cat(format('Ha: mean(diff) < 0', width = 24, justify = 'centre'), format('Ha: mean(diff) ~= 0', width = 24, justify = 'centre'),
    format('Ha: mean(diff) > 0', width = 24, justify = 'centre'), "\n")
  cat(format(all_tval, width = 24, justify = 'centre'), format(all_tval, width = 24, justify = 'centre'), format(all_tval, width = 24, justify = 'centre'), "\n")
  cat(format(all_p_l, width = 24, justify = 'centre'), format(all_p_t, width = 24, justify = 'centre'), format(all_p_u, width = 24, justify = 'centre'), "\n")


  }

}
