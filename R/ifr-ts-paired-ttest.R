#' @title Paired t test
#' @description \code{ifr_ts_paired_ttest} tests that two samples have the
#' same mean, assuming paired data.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param y numeric; column in \code{data}
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter.
#' @return \code{ifr_ts_paired_ttest} returns an object of class \code{"ifr_ts_paired_ttest"}.
#' An object of class \code{"ifr_ts_paired_ttest"} is a list containing the
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
#' \code{ifr_ts_paired_ttest()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#' @examples
#' # lower tail
#' ifr_ts_paired_ttest(hsb, read, write, alternative = 'less')
#'
#' # upper tail
#' ifr_ts_paired_ttest(hsb, read, write, alternative = 'greater')
#'
#' # both tails
#' ifr_ts_paired_ttest(hsb, read, write, alternative = 'both')
#'
#' # all tails
#' ifr_ts_paired_ttest(hsb, read, write, alternative = 'all')
#' @export
#'
ifr_ts_paired_ttest <- function(data, x, y, confint = 0.95,
                                  alternative = c("both", "less", "greater", "all")) UseMethod("ifr_ts_paired_ttest")

#' @export
#'
ifr_ts_paired_ttest.default <- function(data, x, y, confint = 0.95,
                                          alternative = c("both", "less", "greater", "all")) {

  x1   <- deparse(substitute(x))
  y1   <- deparse(substitute(y))
  xone <- data[[x1]]
  yone <- data[[y1]]

  method    <- match.arg(alternative)
  var_names <- names(data[c(x1, y1)])

  k <- paired_comp(xone, yone, confint, var_names)

  result <- list(
    Obs = k$Obs, b = k$b, conf_int1 = k$conf_int1,
    conf_int2 = k$conf_int2, conf_int_diff = k$conf_int_diff, corr = k$corr,
    corsig = k$corsig, tstat = k$tstat, p_lower = k$p_lower,
    p_upper = k$p_upper, p_two_tail = k$p_two_tail, var_names = var_names,
    xy = k$xy, df = k$df, alternative = method, confint = confint
  )

  class(result) <- "ifr_ts_paired_ttest"
  return(result)
}

#' @export
#' @rdname ifr_ts_paired_ttest
#' @usage NULL
#'
infer_ts_paired_ttest <- function(data, x, y, confint = 0.95,
                                  alternative = c("both", "less", "greater", "all")) {
  .Deprecated("ifr_ts_paired_ttest()")
}

#' @export
#'
print.ifr_ts_paired_ttest <- function(x, ...) {
  print_paired_ttest(x)
}

#' @importFrom stats cor
paired_comp <- function(x, y, confint, var_names) {

  n         <- length(x)
  df        <- (n - 1)
  xy        <- paste(var_names[1], "-", var_names[2])

  data_prep <- paired_data(x, y)
  b         <- paired_stats(data_prep, "key", "value")
  corr      <- round(cor(x, y), 4)
  corsig    <- cor_sig(corr, n)

  alpha     <- 1 - confint
  
  confint1  <- conf_int_t(b[[1, 1]], b[[1, 2]], n, alpha = alpha) %>% round(2)
  confint2  <- conf_int_t(b[[2, 1]], b[[2, 2]], n, alpha = alpha) %>% round(2)
  confint3  <- conf_int_t(b[[3, 1]], b[[3, 2]], n, alpha = alpha) %>% round(2)

  t         <- round(b[[3, 1]] / b[[3, 3]], 4)

  p_l       <- pt(t, df)
  p_u       <- pt(t, df, lower.tail = FALSE)
  p         <- pt(abs(t), df, lower.tail = FALSE) * 2

  list(
    Obs = n, b = b, conf_int1 = confint1, conf_int2 = confint2,
    conf_int_diff = confint3, corr = round(corr, 2), corsig = round(corsig, 2),
    tstat = t, p_lower = p_l, p_upper = p_u, p_two_tail = p, xy = xy, df = df
  )

}

paired_data <- function(x, y) {

  j <- data.frame(x = x, y = y)
  j$z <- j$x - j$y
  val <- data.frame(value = c(j$x, j$y, j$z))
  key <- rep(c("x", "y", "z"), each = nrow(j))
  cbind(key = key, value = val)

}

paired_stats <- function(data, key, value) {

  dat <- data.table(data[c("value", "key")])

  out <- dat[, .(length = length(value),
                 mean   = mean(value),
                 sd     = sd(value)),
            by = key]

  out[, ':='(se = sd / sqrt(length))]
  setDF(out)
  out[, c(-1, -2)]

}

cor_sig <- function(corr, n) {

  t   <- corr / ((1 - (corr ^ 2)) / (n - 2)) ^ 0.5
  df  <- n - 2
  sig <- (1 - pt(t, df)) * 2
  round(sig, 4)

}

conf_int_t <- function(u, s, n, alpha = 0.05) {
  
  a     <- alpha / 2
  df    <- n - 1
  error <- round(qt(a, df), 3) * -1
  lower <- u - (error * samp_err(s, n))
  upper <- u + (error * samp_err(s, n))
  c(lower, upper)
  
}

samp_err <- function(sigma, n) {
  sigma / (n ^ 0.5)
}