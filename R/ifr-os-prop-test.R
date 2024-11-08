#' @title One Sample Test of Proportion
#' @description  \code{ifr_os_prop_test} compares proportion in one group to a
#' specified population proportion.
#' @param data numeric vector of length 1 or a \code{data.frame} or \code{tibble}
#' @param variable factor; column in \code{data}
#' @param prob hypothesised proportion
#' @param phat observed proportion
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter.
#'
#' @return \code{ifr_os_prop_test} returns an object of class \code{"ifr_os_prop_test"}.
#' An object of class \code{"ifr_os_prop_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{phat}{proportion of 1's}
#' \item{p}{assumed probability of success}
#' \item{z}{z statistic}
#' \item{sig}{p-value for z statistic}
#' \item{alt}{alternative hypothesis}
#' \item{obs}{observed number of 0's and 1's}
#' \item{exp}{expected number of 0's and 1's}
#' \item{deviation}{deviation of observed from expected}
#' \item{std}{standardized resiudals}
#'
#' @section Deprecated Function:
#' \code{infer_os_prop_test()} has been deprecated. Instead use \code{ifr_os_prop_test()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' # use as a calculator
#' ifr_os_prop_test(200, prob = 0.5, phat = 0.3)
#'
#' # using data set
#' ifr_os_prop_test(hsb, female, prob = 0.5)
#'
#' @seealso \code{\link[stats]{prop.test}} \code{\link[stats]{binom.test}}
#'
#' @export
#'
ifr_os_prop_test <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                             alternative = c("both", "less", "greater", "all"))
  UseMethod("ifr_os_prop_test")

#' @export
#' @rdname ifr_os_prop_test
#'
ifr_os_prop_test.default <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                                     alternative = c("both", "less", "greater", "all")) {
  if (is.numeric(data)) {

    method <- match.arg(alternative)
    k <- prop_comp(data, prob = prob, phat = phat, alternative = method)

  } else {

    varyables <- deparse(substitute(variable))
    fdata     <- data[[varyables]]
    n1        <- length(fdata)
    n2        <- table(fdata)[[2]]
    phat      <- round(n2 / n1, 4)
    prob      <- prob
    method    <- match.arg(alternative)
    k         <- prop_comp(n1, prob = prob, phat = phat, alternative = method)
  }

  result <-
    list(alt       = k$alt,
         deviation = k$deviation,
         exp       = k$exp,
         n         = k$n,
         obs       = k$obs,
         p         = k$p,
         phat      = k$phat,
         sig       = k$sig,
         std       = k$std,
         z         = k$z)

  class(result) <- "ifr_os_prop_test"
  return(result)
}

#' @export
#' @rdname ifr_os_prop_test
#' @usage NULL
#'
infer_os_prop_test <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                               alternative = c("both", "less", "greater", "all")) {
  .Deprecated("ifr_os_prop_test()")
}

#' @export
#'
print.ifr_os_prop_test <- function(x, ...) {
  print_prop_test(x)
}

#' @importFrom stats pnorm
prop_comp <- function(n, prob, alternative, phat) {

  n    <- n
  phat <- phat
  p    <- prob
  q    <- 1 - p
  obs  <- c(n * (1 - phat), n * phat)
  exp  <- n * c(q, p)
  dif  <- obs - exp
  dev  <- round((dif / exp) * 100, 2)
  std  <- round(dif / sqrt(exp), 2)
  num  <- phat - prob
  den  <- sqrt((p * q) / n)
  z    <- round(num / den, 4)
  lt   <- round(pnorm(z), 4)
  ut   <- round(1 - pnorm(z), 4)
  tt   <- round((1 - pnorm(abs(z))) * 2, 4)
  alt  <- alternative

  if (alt == "all") {
    sig <- c("two-both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <-
    list(alt       = alt,
         deviation = format(dev, nsmall = 2),
         exp       = exp,
         n         = n,
         obs       = obs,
         p         = prob,
         phat      = phat,
         sig       = sig,
         std       = format(std, nsmall = 2),
         z         = z)

  return(out)
}
