#' @importFrom stats qnorm
#' @importFrom magrittr %>%
#' @title McNemar Test
#' @description Test if the proportions of two dichotomous variables are
#' equal in the same population.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y factor; column in \code{data}
#' @return \code{infer_mcnemar_test} returns an object of class \code{"infer_mcnemar_test"}.
#' An object of class \code{"infer_mcnemar_test"} is a list containing the
#' following components:
#'
#' \item{statistic}{chi square statistic}
#' \item{df}{degrees of freedom}
#' \item{pvalue}{p-value}
#' \item{exactp}{exact p-value}
#' \item{cstat}{continuity correction chi square statistic}
#' \item{cpvalue}{continuity correction p-value}
#' \item{kappa}{kappa coefficient; measure of interrater agreement}
#' \item{std_err}{asymptotic standard error}
#' \item{kappa_cil}{95\% kappa lower confidence limit}
#' \item{kappa_ciu}{95\% kappa upper confidence limit}
#' \item{cases}{cases}
#' \item{controls}{controls}
#' \item{ratio}{ratio of proportion with factor}
#' \item{odratio}{odds ratio}
#' \item{tbl}{two way table}
#' @section Deprecated Function:
#' \code{mcnermar_test()} has been deprecated. Instead use
#' \code{infer_mcnemar_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @seealso \code{\link[stats]{mcnemar.test}}
#' @examples
#' # using variables from data
#' library(dplyr)
#' hb <- mutate(hsb,
#'         himath = if_else(math > 60, 1, 0),
#'         hiread = if_else(read > 60, 1, 0)
#'     )
#' infer_mcnemar_test(hb, himath, hiread)
#'
#' # test if the proportion of students in himath and hiread group is same
#' himath <- ifelse(hsb$math > 60, 1, 0)
#' hiread <- ifelse(hsb$read > 60, 1, 0)
#' infer_mcnemar_test(table(himath, hiread))
#'
#' # using matrix
#' infer_mcnemar_test(matrix(c(135, 18, 21, 26), nrow = 2))
#' @export
#'
infer_mcnemar_test <- function(data, x = NULL, y = NULL) UseMethod("infer_mcnemar_test")

#' @export
#'
infer_mcnemar_test.default <- function(data, x = NULL, y = NULL) {
  
  if (is.matrix(data) | is.table(data)) {
    dat <- mcdata(data)
  } else {
    x1 <- enquo(x)
    y1 <- enquo(y)

    dat <-
      data %>%
      select(!! x1, !! y1) %>%
      table()
  }

  k <- mccomp(dat)

  result <- list(
    statistic = k$statistic, df = k$df, pvalue = k$pvalue,
    exactp = k$exactp, cstat = k$cstat, cpvalue = k$cpvalue, kappa = k$kappa,
    std_err = k$std_err, kappa_cil = k$kappa_cil, kappa_ciu = k$kappa_ciu,
    cases = k$cases, controls = k$controls, ratio = k$ratio,
    odratio = k$odratio, tbl = dat
  )

  class(result) <- "infer_mcnemar_test"
  return(result)
}

#' @export
#' @rdname infer_mcnemar_test
#' @usage NULL
#'
mcnemar_test <- function(x, y = NULL) {
  .Deprecated("infer_mcnemar_test()")
}

#' @export
#'
print.infer_mcnemar_test <- function(x, ...) {
  print_mcnemar_test(x)
}
