#' @importFrom stats cor
#' @title Paired t test
#' @description \code{infer_ts_paired_ttest} tests that two samples have the
#' same mean, assuming paired data.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param y numeric; column in \code{data}
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter.
#' @return \code{infer_ts_paired_ttest} returns an object of class \code{"infer_ts_paired_ttest"}.
#' An object of class \code{"infer_ts_paired_ttest"} is a list containing the
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
#' @section Deprecated Function:
#' \code{paired_ttest()} has been deprecated. Instead use
#' \code{infer_ts_paired_ttest()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#' @examples
#' # lower tail
#' infer_ts_paired_ttest(hsb, read, write, alternative = 'less')
#'
#' # upper tail
#' infer_ts_paired_ttest(hsb, read, write, alternative = 'greater')
#'
#' # both tails
#' infer_ts_paired_ttest(hsb, read, write, alternative = 'both')
#'
#' # all tails
#' infer_ts_paired_ttest(hsb, read, write, alternative = 'all')
#' @export
#'
infer_ts_paired_ttest <- function(data, x, y, confint = 0.95,
                                  alternative = c("both", "less", "greater", "all")) UseMethod("infer_ts_paired_ttest")

#' @export
#'
infer_ts_paired_ttest.default <- function(data, x, y, confint = 0.95,
                                          alternative = c("both", "less", "greater", "all")) {
  x1 <- enquo(x)
  y1 <- enquo(y)

  method <- match.arg(alternative)

  var_names <-
    data %>%
    select(!! x1, !! y1) %>%
    names()

  xone <-
    data %>%
    pull(!! x1)

  yone <-
    data %>%
    pull(!! y1)

  k <- paired_comp(xone, yone, confint, var_names)

  result <- list(
    Obs = k$Obs, b = k$b, conf_int1 = k$conf_int1,
    conf_int2 = k$conf_int2, conf_int_diff = k$conf_int_diff, corr = k$corr,
    corsig = k$corsig, tstat = k$tstat, p_lower = k$p_lower,
    p_upper = k$p_upper, p_two_tail = k$p_two_tail, var_names = var_names,
    xy = k$xy, df = k$df, alternative = method, confint = confint
  )

  class(result) <- "infer_ts_paired_ttest"
  return(result)
}

#' @export
#' @rdname infer_ts_paired_ttest
#' @usage NULL
#'
paired_ttest <- function(x, y, confint = 0.95,
                         alternative = c("both", "less", "greater", "all")) {
  .Deprecated("infer_ts_paired_ttest()")
  infer_ts_paired_ttest(x, y, confint, alternative)
}

#' @export
#'
print.infer_ts_paired_ttest <- function(x, ...) {
  print_paired_ttest(x)
}
