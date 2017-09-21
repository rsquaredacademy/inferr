#' @title One sample t test
#' @description ttest performs t tests on the equality of means. It tests the
#' hypothesis that \code{x} has a mean of \code{mu}.
#' @param x a numeric vector
#' @param mu a number indicating the true value of the mean
#' @param alpha acceptable tolerance for type I error
#' @param type a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{ttest} returns an object of class \code{"ttest"}.
#' An object of class \code{"ttest"} is a list containing the
#' following components:
#'
#' \item{mu}{a number indicating the true value of the mean}
#' \item{n}{number of observations}
#' \item{df}{degrees of freedom}
#' \item{Mean}{observed mean of \code{x}}
#' \item{stddev}{standard deviation of \code{x}}
#' \item{std_err}{estimate of standard error}
#' \item{test_stat}{t statistic}
#' \item{confint}{confidence interval for the mean}
#' \item{mean_diff}{mean difference}
#' \item{mean_diff_l}{lower confidence limit for mean difference}
#' \item{mean_diff_u}{upper confidence limit for mean difference}
#' \item{p_l}{lower one-sided p-value}
#' \item{p_u}{upper one-sided p-value}
#' \item{p}{two sided p-value}
#' \item{conf}{confidence level}
#' \item{type}{alternative hypothesis}
#' \item{var_name}{name of \code{x}}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#' @examples
#' ttest(hsb$write, mu = 50, type = 'less')
#' ttest(hsb$write, mu = 50, type = 'greater')
#' ttest(hsb$write, mu = 50, type = 'both')
#' ttest(hsb$write, mu = 50, type = 'all')
#' @export
#'
ttest_shiny <- function(data, x, mu = 0, alpha = 0.05,
                  type = c("both", "less", "greater", "all"), ...) UseMethod('ttest_shiny')

#' @export
#'
ttest_shiny.default <- function(data, x, mu = 0, alpha = 0.05,
                  type = c("both", "less", "greater", "all"), ...) {

  if (!is.data.frame(data)) {
    stop('data must be a data frame')
  }

  if (!x %in% colnames(data)) {
    stop('x must be a column in data')
  }

	if (!is.numeric(mu)) {
		stop('mu must be numeric')
	}
	if (!is.numeric(alpha)) {
		stop('alpha must be numeric')
	}

       type <- match.arg(type)
   var_name <- x
          y <- data %>% select_(x) %>% `[[`(1)
          n <- length(y)
          a <- (alpha / 2)
         df <- n - 1
       conf <- 1 - alpha
       Mean <- round(mean(y), 4)
     stddev <- round(sd(y), 4)
    std_err <- round(stddev / sqrt(n), 4)
  test_stat <- round((Mean - mu) / std_err, 3)

  if (type == 'less') {
    cint <- c(-Inf, test_stat + qt(1 - alpha, df) )
  } else if (type == 'greater') {
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


  result <- list(
             mu = mu,
              n = n,
             df = df,
           Mean = Mean,
         stddev = stddev,
        std_err = std_err,
      test_stat = test_stat,
        confint = confint,
      mean_diff = mean_diff,
    mean_diff_l = mean_diff_l,
    mean_diff_u = mean_diff_u,
            p_l = p_l,
            p_u = p_u,
              p = p,
		       conf = conf,
           type = type,
       var_name = var_name)

  class(result) <- 'ttest_shiny'
  return(result)

}

#' @export
#'
print.ttest_shiny <- function(x, ...) {
	print_ttest(x)
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

print_ttest <- function(data) {

	null_l <- paste0("Ho: mean(", data$var_name, ") >=", as.character(data$mu))
  alt_l <- paste0(" Ha: mean(", data$var_name, ") <", as.character(data$mu))
  null_u <- paste0("Ho: mean(", data$var_name, ") <=", as.character(data$mu))
  alt_u <- paste0("Ha: mean(", data$var_name, ") >", as.character(data$mu))
  null_t <- paste0("Ho: mean(", data$var_name, ") ~=", as.character(data$mu))
  alt_t <- paste0("Ha: mean(", data$var_name, ") !=", as.character(data$mu))
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
      formats_t(), formatter_t(data$std_err, sd_width), formats_t(),
      formatter_t(data$stddev, se_width), formats_t(),
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
