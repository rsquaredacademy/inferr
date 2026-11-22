#' @title One Sample Variance Comparison Test
#' @description  \code{ifr_os_var_test} performs tests on the equality of standard
#' deviations (variances).It tests that the standard deviation of a sample is
#' equal to a hypothesized value. Missing values are removed.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x numeric; column in \code{data}
#' @param sd hypothesised standard deviation
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @details Supported inputs: finite data with absolute value \code{<= 1e150}
#' and sample sizes \code{<= 1e15}. At least two non-missing observations are
#' required. Missing values are removed. The standard deviation is computed
#' from a scaled sample when the data magnitude is below \code{1e-100}, so the
#' chi-square statistic and its p-value are invariant to a common rescaling of
#' the data and the hypothesised standard deviation, including scales near the
#' smallest positive doubles.
#'
#' The chi-square statistic is stored to four decimal places and unrounded in
#' \code{chi_raw}; the standard deviation, standard error and confidence limits
#' are stored to four decimal places. p-values are stored unrounded and printed
#' as bounds when they are below the display resolution; an underflowed
#' positive tail is detected from \code{chi_raw} rather than from the rounded
#' statistic.
#'
#' @return \code{ifr_os_var_test} returns an object of class \code{"ifr_os_var_test"}.
#' An object of class \code{"ifr_os_var_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{sd}{hypothesised standard deviation of \code{x}}
#' \item{sigma}{observed standard deviation}
#' \item{se}{estimated standard error}
#' \item{chi}{chi-square statistic, stored to four decimal places}
#' \item{chi_raw}{unrounded chi-square statistic}
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
#' @section Deprecated Function:
#' \code{infer_os_var_test()} has been deprecated. Instead use \code{ifr_os_var_test()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' Snedecor, G. W. and Cochran, W. G. (1989). \emph{Statistical Methods}, 8th ed. Ames, IA: Iowa State University Press.
#'
#' @examples
#' # Is the spread of log grocery spending different from 1? Spending is right-skewed.
#' w <- transform(wholesale_customers, log_grocery = log(grocery))
#'
#' # lower tail
#' ifr_os_var_test(w, log_grocery, 1, alternative = 'less')
#'
#' # upper tail
#' ifr_os_var_test(w, log_grocery, 1, alternative = 'greater')
#'
#' # both tails
#' ifr_os_var_test(w, log_grocery, 1, alternative = 'both')
#'
#' # all tails
#' ifr_os_var_test(w, log_grocery, 1, alternative = 'all')
#'
#' @seealso \code{\link[stats]{var.test}}
#'
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

  if (length(sd) != 1L || !is.finite(sd)) {
    stop("sd must be a finite number greater than 0", call. = FALSE)
  }

  if (sd <= 0) {
    stop("sd must be greater than 0", call. = FALSE)
  }

  check_probability(confint, "confint")

  type    <- match.arg(alternative)
  varname <- names(data[x1])
  k       <- osvar_comp(xone, sd, confint)

  result <-
    list(chi      = round(k$chi, 4),
         chi_raw  = k$chi_raw,
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
#' @rdname ifr_os_var_test
#' @usage NULL
#'
infer_os_var_test <- function(data, x, sd, confint = 0.95,
                              alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("ifr_os_var_test()")
}

#' @export
#'
print.ifr_os_var_test <- function(x, ...) {
  print_os_vartest(x)
}

#' @importFrom stats qchisq
osvar_comp <- function(x, sd, confint) {

  x     <- x[!is.na(x)]
  n     <- length(x)

  if (n < 2) {
    stop("x must contain at least two non-missing observations",
         call. = FALSE)
  }

  check_finite_range(x, "x")
  check_max_sample_size(n, "sample size")

  df    <- n - 1
  xbar  <- mean(x)

  # squaring centred values underflows when the data are extremely small, so
  # compute the scale-invariant standard deviation from a scaled sample
  xscale <- max(abs(x))
  if (is.finite(xscale) && xscale > 0 && xscale < 1e-100) {
    sigma <- stats::sd(x / xscale) * xscale
  } else {
    sigma <- sd(x)
  }

  check_finite_moment(xbar, "mean", "x")
  check_finite_moment(sigma, "standard deviation", "x")

  se    <- sigma / sqrt(n)
  chi   <- df * ((sigma / sd) ^ 2)

  check_finite_moment(se, "standard error", "x")
  check_finite_moment(chi, "chi-square statistic", "x")

  p_lower <- pchisq(chi, df)
  p_upper <- pchisq(chi, df, lower.tail = F)

  if (p_lower < 0.5) {
    p_two <- pchisq(chi, df) * 2
  } else {
    p_two <- pchisq(chi, df, lower.tail = F) * 2
  }

  p_two <- min(1, p_two)

  conf  <- confint
  a     <- (1 - conf) / 2
  al    <- 1 - a
  tv    <- df * sigma ^ 2

  # the confidence limits are sigma * sqrt(df / quantile); the product form
  # stays representable when sigma^2 itself underflows
  if (is.finite(tv) && tv > 0) {
    c_lwr <- round(sqrt(tv / qchisq(al, df)), 4)
    c_upr <- round(sqrt(tv / qchisq(a, df)), 4)
  } else {
    c_lwr <- round(sigma * sqrt(df / qchisq(al, df)), 4)
    c_upr <- round(sigma * sqrt(df / qchisq(a, df)), 4)
  }

  list(chi     = chi,
       chi_raw = chi,
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
