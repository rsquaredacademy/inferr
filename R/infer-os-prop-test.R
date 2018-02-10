#' @importFrom stats pnorm
#' @title One Sample Test of Proportion
#' @description  \code{infer_os_prop_test} compares proportion in one group to a
#' specified population proportion.
#' @param data numeric vector of length 1 or a \code{data.frame} or \code{tibble}
#' @param variable factor; column in \code{data}
#' @param prob hypothesised proportion
#' @param phat observed proportion
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter.
#' @return \code{infer_os_prop_test} returns an object of class \code{"infer_os_prop_test"}.
#' An object of class \code{"infer_os_prop_test"} is a list containing the
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
#' @section Deprecated Function:
#' \code{prop_test()} has been deprecated. Instead use \code{infer_os_prop_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{prop.test}} \code{\link[stats]{binom.test}}
#' @examples
#' # use as a calculator
#' infer_os_prop_test(200, prob = 0.5, phat = 0.3)
#'
#' # using data set
#' infer_os_prop_test(hsb, female, prob = 0.5)
#' @export
#'
infer_os_prop_test <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                               alternative = c("both", "less", "greater", "all"))
  UseMethod("infer_os_prop_test")

#' @export
#' @rdname infer_os_prop_test
#'
infer_os_prop_test.default <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                                       alternative = c("both", "less", "greater", "all")) {
  if (is.numeric(data)) {
    method <- match.arg(alternative)
    k <- prop_comp(
      data, prob = prob, phat = phat,
      alternative = method
    )
  } else {
    varyables <- enquo(variable)

    fdata <-
      data %>%
      pull(!! varyables)

    n1 <- length(fdata)

    n2 <-
      fdata %>%
      table() %>%
      `[[`(2)

    phat <- round(n2 / n1, 4)

    prob <- prob

    method <- match.arg(alternative)

    k <- prop_comp(
      n1, prob = prob, phat = phat,
      alternative = method
    )
  }

  result <- list(
    n = k$n, phat = k$phat, p = k$p, z = k$z, sig = k$sig,
    alt = k$alt, obs = k$obs, exp = k$exp,
    deviation = k$deviation,
    std = k$std
  )

  class(result) <- "infer_os_prop_test"
  return(result)
}


#' @export
#' @rdname infer_os_prop_test
#' @usage NULL
#'
prop_test <- function(n, prob = 0.5,
                      alternative = c("both", "less", "greater", "all"), phat, ...) {
  .Deprecated("infer_os_prop_test()")
}

#' @export
#'
print.infer_os_prop_test <- function(x, ...) {
  print_prop_test(x)
}
