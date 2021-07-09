#' @title One Sample Variance Comparison Test
#' @description  \code{ifr_os_var_test} performs tests on the equality of standard
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
#' @return \code{ifr_os_var_test} returns an object of class \code{"ifr_os_var_test"}.
#' An object of class \code{"ifr_os_var_test"} is a list containing the
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
#' \code{os_vartest()} has been deprecated. Instead use \code{ifr_os_var_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' # lower tail
#' ifr_os_var_test(mtcars, mpg, 5, alternative = 'less')
#'
#' # upper tail
#' ifr_os_var_test(mtcars, mpg, 5, alternative = 'greater')
#'
#' # both tails
#' ifr_os_var_test(mtcars, mpg, 5, alternative = 'both')
#'
#' # all tails
#' ifr_os_var_test(mtcars, mpg, 5, alternative = 'all')
#' @export
#'
ifr_os_var_test <- function(data, x, sd, confint = 0.95,
                              alternative = c("both", "less", "greater", "all"), ...) UseMethod("ifr_os_var_test")

#' @export
#'
ifr_os_var_test.default <- function(data, x, sd, confint = 0.95,
                                      alternative = c("both", "less", "greater", "all"), ...) {

  x1   <- deparse(substitute(x))
  xone <- data[[x1]]

  if (!is.numeric(xone)) {
    stop("x must be numeric", call. = FALSE)
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric", call. = FALSE)
  }

  if (!is.numeric(confint)) {
    stop("confint must be numeric", call. = FALSE)
  }

  type    <- match.arg(alternative)
  varname <- names(data[x1])
  k       <- osvar_comp(xone, sd, confint)

  result <-
    list(chi      = round(k$chi, 4),
         c_lwr    = k$c_lwr,
         conf     = k$conf,
         c_upr    = k$c_upr,
         df       = k$df,
         n        = k$n,
         p_lower  = k$p_lower,
         p_two    = k$p_two,
         p_upper  = k$p_upper,
         sd       = k$sd,
         se       = round(k$se, 4),
         sigma    = round(k$sigma, 4),
         type     = type,
         var_name = varname,
         xbar     = round(k$xbar, 4))

  class(result) <- "ifr_os_var_test"
  return(result)
}

#' @export
#'
print.ifr_os_var_test <- function(x, ...) {
  print_os_vartest(x)
}

#' @importFrom stats qchisq
osvar_comp <- function(x, sd, confint) {

  n     <- length(x)
  df    <- n - 1
  xbar  <- mean(x)
  sigma <- sd(x)
  se    <- sigma / sqrt(n)
  chi   <- df * ((sigma / sd) ^ 2)

  p_lower <- pchisq(chi, df)
  p_upper <- pchisq(chi, df, lower.tail = F)

  if (p_lower < 0.5) {
    p_two <- pchisq(chi, df) * 2
  } else {
    p_two <- pchisq(chi, df, lower.tail = F) * 2
  }

  conf  <- confint
  a     <- (1 - conf) / 2
  al    <- 1 - a
  tv    <- df * sigma
  c_lwr <- round(tv / qchisq(al, df), 4)
  c_upr <- round(tv / qchisq(a, df), 4)

  list(chi     = chi,
       c_lwr   = c_lwr,
       conf    = conf,
       c_upr   = c_upr,
       df      = df,
       n       = n,
       p_lower = p_lower,
       p_two   = p_two,
       p_upper = p_upper,
       sd      = sd,
       se      = se,
       sigma   = sigma,
       xbar    = xbar)

}
