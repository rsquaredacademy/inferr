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
#' # lower tail
#' paired_ttest(hsb$read, hsb$write, alternative = 'less')
#'
#' # upper tail
#' paired_ttest(hsb$read, hsb$write, alternative = 'greater')
#'
#' # both tails
#' paired_ttest(hsb$read, hsb$write, alternative = 'both')
#'
#' # all tails
#' paired_ttest(hsb$read, hsb$write, alternative = 'all')
#' @export
#'
paired_ttest <- function(x, y, confint = 0.95,
  alternative = c('both', 'less', 'greater', 'all')) UseMethod('paired_ttest')

#' @export
#'
paired_ttest.default <- function(x, y, confint = 0.95,
  alternative = c('both', 'less', 'greater', 'all')) {

  if (!is.numeric(x)) {
    stop('x must be numeric')
  }

  if (!is.numeric(y)) {
    stop('y must be numeric')
  }

  if (!is.numeric(confint)) {
    stop('confint must be numeric')
  }

     method <- match.arg(alternative)
      var_x <- l(deparse(substitute(x)))
      var_y <- l(deparse(substitute(y)))
  var_names <- c(var_x, var_y)
          k <- paired_comp(x, y, confint, var_names)

  result <- list(Obs = k$Obs, b = k$b, conf_int1 = k$conf_int1, 
    conf_int2 = k$conf_int2, conf_int_diff = k$conf_int_diff, corr = k$corr, 
    corsig = k$corsig, tstat = k$tstat, p_lower = k$p_lower, 
    p_upper = k$p_upper, p_two_tail = k$p_two_tail, var_names = var_names,
    xy = k$xy, df = k$df, alternative = method, confint = confint)

  class(result) <- 'paired_ttest'
  return(result)
}

#' @export
#'
print.paired_ttest <- function(x, ...) {
  print_paired_ttest(x)
}
