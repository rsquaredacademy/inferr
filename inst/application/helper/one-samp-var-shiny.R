#' @importFrom stats qchisq
#' @title One sample variance comparison test
#' @description  \code{os_vartest} performs tests on the equality of standard
#' deviations (variances).It tests that the standard deviation of \code{x} is
#' \code{sd}.
#' @param x a numeric vector
#' @param sd hypothesised standard deviation
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{os_vartest} returns an object of class \code{"os_vartest"}.
#' An object of class \code{"os_vartest"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{sd}{hypothesised standard deviation of \code{x}}
#' \item{sigma}{observed standard deviation}
#' \item{se}{estimated standard error}
#' \item{chi}{chi-square statistic}
#' \item{df}{degrees of freedom}
#' \item{p_lower}{lower one-sided p-value}
#' \item{p_upper}{upper one-sided p-value}
#' \item{p_two}{two-sided p-value}
#' \item{xbar}{mean of \code{x}}
#' \item{c_lwr}{lower confidence limit of standard deviation}
#' \item{c_upr}{upper confidence limit of standard deviation}
#' \item{var_name}{name of \code{x}}
#' \item{conf}{confidence level}
#' \item{type}{alternative hypothesis}
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' os_vartest(mtcars$mpg, 5, alternative = 'less')
#' os_vartest(mtcars$mpg, 5, alternative = 'greater')
#' os_vartest(mtcars$mpg, 5, alternative = 'both')
#' os_vartest(mtcars$mpg, 5, alternative = 'all')
#'
#' @export
#'
os_vartest_shiny <- function(data, x, sd, confint = 0.95,
	alternative = c('both', 'less', 'greater', 'all'), ...) UseMethod('os_vartest_shiny')

#' @export
#'
os_vartest_shiny.default <- function(data, x, sd, confint = 0.95,
	alternative = c('both', 'less', 'greater', 'all'), ...) {

	if (!is.numeric(sd)) {
		stop('sd must be numeric')
	}

	if (!is.numeric(confint)) {
		stop('confint must be numeric')
	}

	   type <- match.arg(alternative)
	varname <- x
       x1 <- data %>% select_(x) %>% `[[`(1)
	      n <- length(x1)
	     df <- n - 1
	   xbar <- round(mean(x1), 4)
	  sigma <- round(sd(x1), 4)
	     se <- round(sigma / sqrt(n), 4)
	    chi <- round((df * (sigma / sd) ^ 2), 4)

	p_lower <- pchisq(chi, df)
	p_upper <- pchisq(chi, df, lower.tail = F)
	if (p_lower < 0.5) {
			p_two <- pchisq(chi, df) * 2
	} else {
			p_two   <- pchisq(chi, df, lower.tail = F) * 2
	}


	 conf <- confint
	    a <- (1 - conf) / 2
	   al <- 1 - a
	   tv <- df * sigma
	c_lwr <- round(tv / qchisq(al, df), 4)
	c_upr <- round(tv / qchisq(a, df), 4)

	result <- list(
		       n = n,
		      sd = sd,
		   sigma = sigma,
		      se = se,
		     chi = chi,
		      df = df,
		 p_lower = p_lower,
		 p_upper = p_upper,
		   p_two = p_two,
		    xbar = xbar,
			 c_lwr = c_lwr,
			 c_upr = c_upr,
		var_name = varname,
		    conf = conf,
		    type = type)

	class(result) <- 'os_vartest_shiny'
	return(result)

}

#' @export
#'
print.os_vartest_shiny <- function(x, ...) {
  print_os_vartest(x)
}


print_os_vartest <- function(data) {

	null_l <- paste0("Ho: sd(", data$var_name, ") >= ", as.character(data$sd))
  alt_l <- paste0(" Ha: sd(", data$var_name, ") < ", as.character(data$sd))
  null_u <- paste0("Ho: sd(", data$var_name, ") <= ", as.character(data$sd))
  alt_u <- paste0("Ha: sd(", data$var_name, ") > ", as.character(data$sd))
  null_t <- paste0("Ho: sd(", data$var_name, ") = ", as.character(data$sd))
  alt_t <- paste0("Ha: sd(", data$var_name, ") != ", as.character(data$sd))
  all_l <- paste("Ha: sd <", as.character(data$sd))
  all_u <- paste("Ha: sd >", as.character(data$sd))
  all_t <- paste("Ha: sd !=", as.character(data$sd))
  char_p_l <- format(data$p_lower, digits = 0, nsmall = 4)
  char_p_u <- format(data$p_upper, digits = 0, nsmall = 4)
  char_p <- format(data$p_two, digits = 0, nsmall = 4)
  all_p_l <- paste("Pr(C < c) =", char_p_l)
	if (data$p_lower < 0.5) {
			all_p_t <- paste("2 * Pr(C < c) =", char_p)
	} else {
			all_p_t <- paste("2 * Pr(C > c) =", char_p)
	}
  all_p_u <- paste("Pr(C > c) =", char_p_u)
  all_tval <- paste0(" c = ", as.character(data$chi))


  # formatting output
  # compute the characters of each output and decide the overall width
  var_width <- max(nchar('Variable'), nchar(data$var_name))
  obs_width <- max(nchar('Obs'), nchar(data$n))
  mean_width <- max(nchar('Mean'), nchar(data$xbar))
  se_width <- max(nchar('Std. Err.'), nchar(data$se))
  sd_width <- max(nchar('Std. Dev.'), nchar(data$sigma))
  conf_length <- nchar(data$c_lwr) + nchar(data$c_upr)
  conf_str <- paste0('[', data$conf * 100, '% Conf. Interval]')
  confint_length <- nchar(conf_str)
  if (conf_length > confint_length) {
    conf_width <- round(conf_length / 2)
  } else {
    conf_width <- round(confint_length / 2)
  }
  c_width <- nchar(data$chi)
  df_width <- max(nchar('DF'), nchar(data$df))
  p_width <- max(nchar('2 Tailed'), nchar(round(data$p_two, 5)))
  md_width <- max(nchar('Difference'), nchar(data$mean_diff))
  md_length <- nchar(data$mean_diff_l) + nchar(data$mean_diff_u)

  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 21)
  width_2 <- sum(var_width, c_width, df_width, p_width, 12)
  all_width <- round(width_1 / 3)
	width_3 <- all_width * 3

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
      formatter_t(data$xbar, mean_width),
      formats_t(), formatter_t(data$se, se_width), formats_t(),
      formatter_t(data$sigma, sd_width), formats_t(),
      format_cil(data$c_lwr, conf_width),
      format_ciu(data$c_upr, conf_width), "\n")
    cat(rep("-", width_1), sep = "")

  # print result
  if (data$type == "less") {

    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_l, width = width_2, justify = "centre"))
    cat("\n", format(alt_l, width = width_2, justify = "centre"), "\n\n")
    cat(format('Chi-Square Test for Variance', width = width_2, justify = 'centre'), '\n')
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p_l, p_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else if (data$type == "greater") {

    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_u, width = width_2, justify = "centre"))
    cat("\n", format(alt_u, width = width_2, justify = "centre"), "\n\n")
    cat(format('Chi-Square Test for Variance', width = width_2, justify = 'centre'), '\n')
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p_u, p_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else if (data$type == "both") {

    cat("\n\n", format("Two Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n", format(alt_t, width = width_2, justify = "centre"), "\n\n")
    cat(format('Chi-Square Test for Variance', width = width_2, justify = 'centre'), '\n')
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), "\n")
    cat(rep("-", width_2), sep = "")
    cat("\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p, p_width), "\n")
    cat(rep("-", width_2), sep = "")

  } else {

    cat("\n\n", format(null_t, width = width_3, justify = "centre"))
    cat("\n\n", format(all_l, width = all_width, justify = "centre"), format(all_t, width = all_width, justify = "centre"), format(all_u, width = all_width, justify = "centre"), "\n")
    cat(format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'), format(all_tval, width = all_width, justify = 'centre'))
    cat("\n", format(all_p_l, width = all_width, justify = 'centre'), format(all_p_t, width = all_width, justify = 'centre'), format(all_p_u, width = all_width, justify = 'centre'))

  }

}
