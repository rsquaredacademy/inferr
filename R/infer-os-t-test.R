#' @title One Sample t Test
#' @description \code{infer_os_t_test} performs t tests on the equality of means. It tests the
#' hypothesis that a sample has a mean equal to a hypothesized value.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param mu a number indicating the true value of the mean
#' @param alpha acceptable tolerance for type I error
#' @param alternative a character string specifying the alternative hypothesis, must be
#' one of "both" (default), "greater", "less" or "all". You can specify just the
#' initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{infer_os_t_test} returns an object of class \code{"infer_os_t_test"}.
#' An object of class \code{"infer_os_t_test"} is a list containing the
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
#' @section Deprecated Function:
#' \code{ttest()} has been deprecated. Instead use \code{infer_os_t_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#'
#' @examples
#' # lower tail
#' infer_os_t_test(hsb, write, mu = 50, alternative = 'less')
#'
#' # upper tail
#' infer_os_t_test(hsb, write, mu = 50, alternative = 'greater')
#'
#' # both tails
#' infer_os_t_test(hsb, write, mu = 50, alternative = 'both')
#'
#' # all tails
#' infer_os_t_test(hsb, write, mu = 50, alternative = 'all')
#' @export
#'
infer_os_t_test <- function(data, x, mu = 0, alpha = 0.05,
                            alternative = c("both", "less", "greater", "all"), ...) UseMethod("infer_os_t_test")

#' @export
#'
infer_os_t_test.default <- function(data, x, mu = 0, alpha = 0.05,
                                    alternative = c("both", "less", "greater", "all"), ...) {
  
  x1   <- enquo(x)
  xone <- pull(data, !! x1)

  if (!is.numeric(xone)) {
    stop("x must be numeric")
  }
  if (!is.numeric(mu)) {
    stop("mu must be numeric")
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric")
  }

  type <- match.arg(alternative)

  var_name <-
    data %>%
    select(!! x1) %>%
    names()

  k <- ttest_comp(xone, mu, alpha, type)

  result <- list(
    mu = k$mu, n = k$n, df = k$df, Mean = k$Mean,
    stddev = k$stddev, std_err = k$std_err,
    test_stat = k$test_stat, confint = k$confint,
    mean_diff = k$mean_diff, mean_diff_l = k$mean_diff_l,
    mean_diff_u = k$mean_diff_u, p_l = k$p_l, p_u = k$p_u,
    p = k$p, conf = k$conf, type = type, var_name = var_name
  )

  class(result) <- "infer_os_t_test"
  return(result)
}

#' @export
#' @rdname infer_os_t_test
#' @usage NULL
#'
ttest <- function(x, mu = 0, alpha = 0.05,
                  type = c("both", "less", "greater", "all"), ...) {
  .Deprecated("infer_os_t_test()")
}

#' @export
#'
print.infer_os_t_test <- function(x, ...) {
  print_ttest(x)
}
