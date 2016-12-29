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

        # pearson chi square
        twoway <- matrix(table(x, y), nrow = 2)
        mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
        mat2 <- matrix(colSums(twoway), nrow = 1)
        ef <- mat1 %*% mat2
        chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
        df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
        sig <- round(pchisq(chi, df, lower.tail = F), 4)

        # likelihood ratio chi square
        chilr <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
        sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)

        # Yates continuity correction
        way2 <- twoway[, c(2, 1)]
        total <- sum(twoway)
        prods <- prod(diag(twoway)) - prod(diag(way2))
        prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
        chi_y <- round((total *  (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
        sig_y <- round(pchisq(chi_y, 1, lower.tail = F), 4)

        # mantel haenszel chi square
        num <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
        den <- prod_totals / ((total ^ 3) - (total ^ 2))
        chimh <- round((num ^ 2) / den, 4)
        sig_mh <- round(pchisq(chimh, 1, lower.tail = F), 4)

    } else {

        twoway <- matrix(table(x, y), nrow = dk[1])
        mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = dk[1])
        mat2 <- matrix(colSums(twoway), ncol = dk[2])
        ef <- mat1 %*% mat2
        chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
        df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
        sig <- round(pchisq(chi, df, lower.tail = F), 4)

        # likelihood ratio chi square
        chilr <- round(2 * sum(matrix(twoway, ncol = ds) %*% matrix(log(twoway / ef), nrow = ds)), 4)
        sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)

    }

    # # pearson chi square
    twoway <- matrix(table(x, y), nrow = nlevels(as.factor(x)),
                    ncol = nlevels(as.factor(y)))
    # mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
    # mat2 <- matrix(colSums(twoway), nrow = 1)
    # ef <- mat1 %*% mat2
    # chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
    # df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
    # sig <- round(pchisq(chi, df, lower.tail = F), 4)

    # # likelihood ratio chi square
    # chilr <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
    # sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)

    # # Yates continuity correction
    # way2 <- twoway[, c(2, 1)]
    total <- sum(twoway)
    # prods <- prod(diag(twoway)) - prod(diag(way2))
    # prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
    # chi_y <- round((total *  (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
    # sig_y <- round(pchisq(chi_y, 1, lower.tail = F), 4)

    # # mantel haenszel chi square
    # num <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
    # den <- prod_totals / ((total ^ 3) - (total ^ 2))
    # chimh <- round((num ^ 2) / den, 4)
    # sig_mh <- round(pchisq(chimh, 1, lower.tail = F), 4)


    # phi coefficient
    phi <- round(sqrt(chi / total), 4)

    # contingency coefficient
    cc <- round(sqrt(chi / (chi + total)), 4)

    # cramer's v
    q <- min(nrow(twoway), ncol(twoway))
    cv <- round(sqrt(chi / (total * (q - 1))), 4)

    result <- if (ds == 4) {
      list(chi = chi,
           chilr = chilr,
           chimh = chimh,
           chiy = chi_y,
           sig = sig,
           siglr = sig_lr,
           sigy = sig_y,
           sigmh = sig_mh,
           phi = phi,
           cc = cc,
           cv = cv,
           ds = ds,
           df = df)
    } else {
      list(df = df,
           chi = chi,
           chilr = chilr,
           sig = sig,
           siglr = sig_lr,
           phi = phi,
           cc = cc,
           cv = cv,
           ds = ds)
    }

    class(result) <- 'chisq_test'
    return(result)

}

#' @export
print.chisq_test <- function(x, ...) {
  print_chisq_test(x)
}
