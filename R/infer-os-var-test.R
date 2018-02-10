#' @importFrom stats qchisq
#' @title One Sample Variance Comparison Test
#' @description  \code{infer_os_var_test} performs tests on the equality of standard
#' deviations (variances).It tests that the standard deviation of a sample is
#' equal to a hypothesized value.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param sd hypothesised standard deviation
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{infer_os_var_test} returns an object of class \code{"infer_os_var_test"}.
#' An object of class \code{"infer_os_var_test"} is a list containing the
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
#' @section Deprecated Function:
#' \code{os_vartest()} has been deprecated. Instead use \code{infer_os_var_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' # lower tail
#' infer_os_var_test(mtcars, mpg, 5, alternative = 'less')
#'
#' # upper tail
#' infer_os_var_test(mtcars, mpg, 5, alternative = 'greater')
#'
#' # both tails
#' infer_os_var_test(mtcars, mpg, 5, alternative = 'both')
#'
#' # all tails
#' infer_os_var_test(mtcars, mpg, 5, alternative = 'all')
#' @export
#'
infer_os_var_test <- function(data, x, sd, confint = 0.95,
                              alternative = c("both", "less", "greater", "all"), ...) UseMethod("infer_os_var_test")

#' @export
#'
infer_os_var_test.default <- function(data, x, sd, confint = 0.95,
                                      alternative = c("both", "less", "greater", "all"), ...) {
  x1 <- enquo(x)

  xone <-
    data %>%
    pull(!! x1)

  if (!is.numeric(xone)) {
    stop("x must be numeric")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric")
  }

  if (!is.numeric(confint)) {
    stop("confint must be numeric")
  }

  type <- match.arg(alternative)

  varname <-
    data %>%
    select(!! x1) %>%
    names()

  k <- osvar_comp(xone, sd, confint)

  result <- list(
    n = k$n, sd = k$sd, sigma = k$sigma, se = k$se, chi = k$chi,
    df = k$df, p_lower = k$p_lower, p_upper = k$p_upper, p_two = k$p_two,
    xbar = k$xbar, c_lwr = k$c_lwr, c_upr = k$c_upr, var_name = varname,
    conf = k$conf, type = type
  )

  class(result) <- "infer_os_var_test"
  return(result)
}

#' @export
#' @rdname infer_os_var_test
#' @usage NULL
#'
os_vartest <- function(x, sd, confint = 0.95,
                       alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("infer_os_var_test()")
}

#' @export
#'
print.infer_os_var_test <- function(x, ...) {
  print_os_vartest(x)
}
