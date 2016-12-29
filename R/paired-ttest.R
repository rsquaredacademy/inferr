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
#' @examples
#' paired_ttest(mtcars$mpg, mtcars$qsec, alternative = 'less')
#' paired_ttest(mtcars$mpg, mtcars$qsec, alternative = 'greater')
#' paired_ttest(mtcars$mpg, mtcars$qsec, alternative = 'both')
#' paired_ttest(mtcars$mpg, mtcars$qsec, alternative = 'all')
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
  n <- length(x)
  df <- (n - 1)
  xy <- paste(var_names[1], '-', var_names[2])
  trial <- extract(x, y)
  a <- sapply(trial, stat)
  b <- apply(a, c(1, 2), round, 2)
  corr <- round(cor(x, y), 4)
  corsig <- cor_sig(corr, n)
  alpha <- 1 - confint
  confint1 <- conf_int_t(b[1, 1], b[2, 1], n, alpha = alpha)
  conf_int1 <- lapply(confint1, round, 2)
  confint2 <- conf_int_t(b[1, 2], b[2, 2], n, alpha = alpha)
  conf_int2 <- lapply(confint2, round, 2)
  confint3 <- conf_int_t(b[1, 3], b[2, 3], n, alpha = alpha)
  conf_int3 <- lapply(confint3, round, 2)
  t <- round(b[1, 3] / b[3, 3], 4)
  p_l <- pt(t, df)
  p_u <- pt(t, df, lower.tail = FALSE)
  p <- pt(abs(t), df, lower.tail = FALSE) * 2

  result <- list(Obs           = n,
                 b             = b,
                 conf_int1     = conf_int1,
                 conf_int2     = conf_int2,
                 conf_int_diff = conf_int3,
                 corr          = round(corr, 2),
                 corsig        = round(corsig, 2),
                 tstat         = t,
                 p_lower       = p_l,
                 p_upper       = p_u,
                 p_two_tail    = p,
                 var_names     = var_names,
                 xy            = xy,
                 df            = df,
                 alternative   = method,
                 confint       = confint)

  class(result) <- 'paired_ttest'
  return(result)
}

#' @export
#'
print.paired_ttest <- function(x, ...) {
  print_paired_ttest(x)
}
