#' @title Chi Square Test of Association
#' @description Chi Square test of association to examine if there is a
#' relationship between two categorical variables.
#' @param data a \code{data.frame} or \code{tibble}
#' @param x factor; column in \code{data}
#' @param y factor; column in \code{data}
#' @return \code{infer_chisq_assoc_test} returns an object of class
#' \code{"infer_chisq_assoc_test"}. An object of class
#' \code{"infer_chisq_assoc_test"} is a list containing the
#' following components:
#'
#' \item{chi}{chi square}
#' \item{chilr}{likelihood ratio chi square}
#' \item{chimh}{mantel haenszel chi square}
#' \item{chiy}{continuity adjusted chi square}
#' \item{sig}{p-value of chi square}
#' \item{siglr}{p-value of likelihood ratio chi square}
#' \item{sigmh}{p-value of mantel haenszel chi square}
#' \item{sigy}{p-value of continuity adjusted chi square}
#' \item{phi}{phi coefficient}
#' \item{cc}{contingency coefficient}
#' \item{cv}{cramer's v}
#' \item{ds}{product of dimensions of the table of \code{x} and \code{y}}
#' \item{df}{degrees of freedom}
#' @section Deprecated Function:
#' \code{chisq_test()} has been deprecated. Instead use
#' \code{infer_chisq_assoc_test()}.
#'
#' @seealso \code{\link[stats]{chisq.test}}
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @examples
#' infer_chisq_assoc_test(hsb, female, schtyp)
#'
#' infer_chisq_assoc_test(hsb, female, ses)
#' @export
#'
infer_chisq_assoc_test <- function(data, x, y) UseMethod("infer_chisq_assoc_test")

#' @export
infer_chisq_assoc_test.default <- function(data, x, y) {
  
  x1 <- rlang::enquo(x)
  y1 <- rlang::enquo(y)

  xone <- dplyr::pull(data, !! x1)
  yone <- dplyr::pull(data, !! y1)

  if (!is.factor(xone)) {
    stop("x must be a categorical variable")
  }

  if (!is.factor(yone)) {
    stop("y must be a categorical variable")
  }

  # dimensions
  k  <- table(xone, yone)
  dk <- dim(k)
  ds <- prod(dk)
  nr <- dk[1]
  nc <- dk[2]


  if (ds == 4) {
    twoway <- matrix(table(xone, yone), nrow = 2)
    df <- df_chi(twoway)
    ef <- efmat(twoway)
    k  <- pear_chsq(twoway, df, ef)
    m  <- lr_chsq(twoway, df, ef)
    n  <- yates_chsq(twoway)
    p  <- mh_chsq(twoway, n$total, n$prod_totals)
  } else {
    twoway <- matrix(table(xone, yone), nrow = dk[1])
    ef <- efm(twoway, dk)
    df <- df_chi(twoway)
    k  <- pear_chi(twoway, df, ef)
    m  <- lr_chsq2(twoway, df, ef, ds)
  }

  j <- chigf(xone, yone, k$chi)

  result <- if (ds == 4) {
    list(
      chi = k$chi, chilr = m$chilr, chimh = p$chimh, chiy = n$chi_y,
      sig = k$sig, siglr = m$sig_lr, sigy = n$sig_y, sigmh = p$sig_mh,
      phi = j$phi, cc = j$cc, cv = j$cv, ds = ds, df = df
    )
  } else {
    list(
      df = df, chi = k$chi, chilr = m$chilr, sig = k$sig, siglr = m$sig_lr,
      phi = j$phi, cc = j$cc, cv = j$cv, ds = ds
    )
  }

  class(result) <- "infer_chisq_assoc_test"
  return(result)
}

#' @export
#' @rdname infer_chisq_assoc_test
#' @usage NULL
#'
chisq_test <- function(x, y) {
  .Deprecated("infer_chisq_assoc_test()")
}

#' @export
print.infer_chisq_assoc_test <- function(x, ...) {
  print_chisq_test(x)
}
