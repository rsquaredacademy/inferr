#' @title One Sample t Test
#' @description \code{ttest} performs t tests on the equality of means. It tests the
#' hypothesis that a sample has a mean equal to a hypothesized value.
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
#'
#' @examples
#' # lower tail
#' ttest(hsb$write, mu = 50, type = 'less')
#'
#' # upper tail
#' ttest(hsb$write, mu = 50, type = 'greater')
#'
#' # both tails
#' ttest(hsb$write, mu = 50, type = 'both')
#'
#' # all tails
#' ttest(hsb$write, mu = 50, type = 'all')
#' @export
#'
ttest <- function(x, mu = 0, alpha = 0.05,
                  type = c("both", "less", "greater", "all"), ...) UseMethod('ttest')

#' @export
#'
ttest.default <- function(x, mu = 0, alpha = 0.05,
                  type = c("both", "less", "greater", "all"), ...) {

	if (!is.numeric(x)) {
		stop('x must be numeric')
	}
	if (!is.numeric(mu)) {
		stop('mu must be numeric')
	}
	if (!is.numeric(alpha)) {
		stop('alpha must be numeric')
	}

       type <- match.arg(type)
   var_name <- l(deparse(substitute(x)))
          k <- ttest_comp(x, mu, alpha, type)
  #         n <- length(x)
  #         a <- (alpha / 2)
  #        df <- n - 1
  #      conf <- 1 - alpha
  #      Mean <- round(mean(x), 4)
  #    stddev <- round(sd(x), 4)
  #   std_err <- round(stddev / sqrt(n), 4)
  # test_stat <- round((Mean - mu) / std_err, 3)

  # if (type == 'less') {
  #   cint <- c(-Inf, test_stat + qt(1 - alpha, df) )
  # } else if (type == 'greater') {
  #   cint <- c(test_stat - qt(1 - alpha, df), Inf)
  # } else {
  #   cint <- qt(1 - a, df)
  #   cint <- test_stat + c(-cint, cint)
  # }

  #     confint <- round(mu + cint * std_err, 4)
  #   mean_diff <- round((Mean - mu), 4)
  # mean_diff_l <- confint[1] - mu
  # mean_diff_u <- confint[2] - mu
  #         p_l <- pt(test_stat, df)
  #         p_u <- pt(test_stat, df, lower.tail = FALSE)

  # if (p_l < 0.5) {
  #   p <- p_l * 2
  # } else {
  #   p <- p_u * 2
  # }


  result <- list(mu = k$mu, n = k$n, df = k$df, Mean = k$Mean, stddev = k$stddev, 
    std_err = k$std_err, test_stat = k$test_stat, confint = k$confint, 
    mean_diff = k$mean_diff, mean_diff_l = k$mean_diff_l, 
    mean_diff_u = k$mean_diff_u, p_l = k$p_l, p_u = k$p_u, p = k$p, conf = k$conf, 
    type = type, var_name = var_name)

  class(result) <- 'ttest'
  return(result)

}

#' @export
#'
print.ttest <- function(x, ...) {
	print_ttest(x)
}
