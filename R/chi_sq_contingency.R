#' @importFrom stats pchisq
#' @title Chi Square Test
#' @description Chi Square contingency table test to examine if there is a
#' relationship between two categorical variables.
#' @param x a categorical variable
#' @param y a categorical variable
#' @return \code{chisq_test} returns an object of class \code{"chisq_test"}.
#' An object of class \code{"chisq_test"} is a list containing the
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
#'
#' @seealso \code{\link[stats]{chisq.test}}
#' @examples
#' chisq_test(as.factor(hsb$female), as.factor(hsb$schtyp))
#' chisq_test(as.factor(hsb$female), as.factor(hsb$ses))
#' @export
#'
chisq_test <- function(x, y) UseMethod('chisq_test')

#' @export
chisq_test.default <- function(x, y) {

    if (!is.factor(x)) {
      stop('x must be a categorical variable')
    }

    if (!is.factor(y)) {
      stop('y must be a categorical variable')
    }

    # dimensions
    k <- table(x, y)
    dk <- dim(k)
    ds <- prod(dk)
    nr <- dk[1]
    nc <- dk[2]


    if (ds == 4) {

        twoway <- matrix(table(x, y), nrow = 2)
            df <- df_chi(twoway)
            ef <- efmat(twoway)
             k <- pear_chsq(twoway, df, ef)
             m <- lr_chsq(twoway, df, ef)
             n <- yates_chsq(twoway)
             p <- mh_chsq(twoway, n$total, n$prod_totals)
        
    } else {

        twoway <- matrix(table(x, y), nrow = dk[1])
            ef <- efm(twoway, dk)
            df <- df_chi(twoway)
             k <- pear_chi(twoway, df, ef)
             m <- lr_chsq2(twoway, df, ef, ds)

    }

    j <- chigf(x, y, k$chi)

    result <- if (ds == 4) {
      list(chi = k$chi, chilr = m$chilr, chimh = p$chimh, chiy = n$chi_y,
           sig = k$sig, siglr = m$sig_lr, sigy = n$sig_y, sigmh = p$sig_mh,
           phi = j$phi, cc = j$cc, cv = j$cv, ds = ds, df = df)
    } else {
      list(df = df, chi = k$chi, chilr = m$chilr, sig = k$sig, siglr = m$sig_lr,
           phi = j$phi, cc = j$cc, cv = j$cv, ds = ds)
    }

    class(result) <- 'chisq_test'
    return(result)

}

#' @export
print.chisq_test <- function(x, ...) {
  print_chisq_test(x)
}
