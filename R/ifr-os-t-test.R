#' @title One Sample t Test
#' @description \code{ifr_os_t_test} performs t tests on the equality of means. It tests the
#' hypothesis that a sample has a mean equal to a hypothesized value.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param mu a number indicating the true value of the mean
#' @param alpha acceptable tolerance for type I error
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter
#' @param ... additional arguments passed to or from other methods
#'
#' @return \code{ifr_os_t_test} returns an object of class \code{"ifr_os_t_test"}.
#' An object of class \code{"ifr_os_t_test"} is a list containing the
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
#' @section Deprecated Function:
#' \code{infer_os_t_test()} has been deprecated. Instead use \code{ifr_os_t_test()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' # lower tail
#' ifr_os_t_test(hsb, write, mu = 50, alternative = 'less')
#'
#' # upper tail
#' ifr_os_t_test(hsb, write, mu = 50, alternative = 'greater')
#'
#' # both tails
#' ifr_os_t_test(hsb, write, mu = 50, alternative = 'both')
#'
#' # all tails
#' ifr_os_t_test(hsb, write, mu = 50, alternative = 'all')
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @export
#'
ifr_os_t_test <- function(data, x, mu = 0, alpha = 0.05,
                            alternative = c("both", "less", "greater", "all"), ...) UseMethod("ifr_os_t_test")

#' @export
#'
ifr_os_t_test.default <- function(data, x, mu = 0, alpha = 0.05,
                                    alternative = c("both", "less", "greater", "all"), ...) {

  x1   <- deparse(substitute(x))
  xone <- data[[x1]]

  if (!is.numeric(xone)) {
    stop("x must be numeric", call. = FALSE)
  }
  if (!is.numeric(mu)) {
    stop("mu must be numeric", call. = FALSE)
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric", call. = FALSE)
  }

  type     <- match.arg(alternative)
  var_name <- names(data[x1])
  k        <- ttest_comp(xone, mu, alpha, type)

  result <-
    list(conf        = k$conf,
         confint     = k$confint,
         df          = k$df,
         Mean        = k$Mean,
         mean_diff   = k$mean_diff,
         mean_diff_l = k$mean_diff_l,
         mean_diff_u = k$mean_diff_u,
         mu          = k$mu,
         n           = k$n,
         p           = k$p,
         p_l         = k$p_l,
         p_u         = k$p_u,
         stddev      = k$stddev,
         std_err     = k$std_err,
         test_stat   = k$test_stat,
         type        = type,
         var_name    = var_name)

  class(result) <- "ifr_os_t_test"
  return(result)
}

#' @export
#' @rdname ifr_os_t_test
#' @usage NULL
#'
infer_os_t_test <- function(data, x, mu = 0, alpha = 0.05,
                            alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("ifr_os_t_test()")
}

#' @export
#'
print.ifr_os_t_test <- function(x, ...) {
  print_ttest(x)
}

#' @importFrom stats qt pt
ttest_comp <- function(x, mu, alpha, type) {

  n         <- length(x)
  a         <- (alpha / 2)
  df        <- n - 1
  conf      <- 1 - alpha
  Mean      <- round(mean(x), 4)
  stddev    <- round(sd(x), 4)
  std_err   <- round(stddev / sqrt(n), 4)
  test_stat <- round((Mean - mu) / std_err, 3)

  if (type == "less") {
    cint <- c(-Inf, test_stat + qt(1 - alpha, df))
  } else if (type == "greater") {
    cint <- c(test_stat - qt(1 - alpha, df), Inf)
  } else {
    cint <- qt(1 - a, df)
    cint <- test_stat + c(-cint, cint)
  }

  confint     <- round(mu + cint * std_err, 4)
  mean_diff   <- round((Mean - mu), 4)
  mean_diff_l <- confint[1] - mu
  mean_diff_u <- confint[2] - mu
  p_l         <- pt(test_stat, df)
  p_u         <- pt(test_stat, df, lower.tail = FALSE)

  if (p_l < 0.5) {
    p <- p_l * 2
  } else {
    p <- p_u * 2
  }


  out <-
    list(conf        = conf,
         confint     = confint,
         df          = df,
         Mean        = Mean,
         mean_diff   = mean_diff,
         mean_diff_l = mean_diff_l,
         mean_diff_u = mean_diff_u,
         mu          = mu,
         n           = n,
         p           = p,
         p_l         = p_l,
         p_u         = p_u,
         stddev      = stddev,
         std_err     = std_err,
         test_stat   = test_stat)

  return(out)
}
