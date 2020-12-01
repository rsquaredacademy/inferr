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
#' \item{chisquare}{chi square}
#' \item{chisquare_lr}{likelihood ratio chi square}
#' \item{chisquare_mantel_haenszel}{mantel haenszel chi square}
#' \item{chisquare_adjusted}{continuity adjusted chi square}
#' \item{contingency_coefficient}{contingency coefficient}
#' \item{cramers_v}{cramer's v}
#' \item{df}{degrees of freedom}
#' \item{ds}{product of dimensions of the table of \code{x} and \code{y}}
#' \item{phi_coefficient}{phi coefficient}
#' \item{pval_chisquare}{p-value of chi square}
#' \item{pval_chisquare_adjusted}{p-value of continuity adjusted chi square}
#' \item{pval_chisquare_lr}{p-value of likelihood ratio chi square}
#' \item{pval_chisquare_mantel_haenszel}{p-value of mantel haenszel chi square}
#'
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

  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))

  xone <- data[[x1]]
  yone <- data[[y1]]

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
      chisquare                      = k$chi,
      chisquare_adjusted             = n$chi_y,
      chisquare_lr                   = m$chilr,
      chisquare_mantel_haenszel      = p$chimh,
      contingency_coefficient        = j$cc,
      cramers_v                      = j$cv,
      df                             = df,
      ds                             = ds,
      phi_coefficient                = j$phi,
      pval_chisquare                 = k$sig,
      pval_chisquare_adjusted        = n$sig_y,
      pval_chisquare_lr              = m$sig_lr,
      pval_chisquare_mantel_haenszel = p$sig_mh
    )
  } else {
    list(
      chisquare               = k$chi,
      chisquare_lr            = m$chilr,
      contingency_coefficient = j$cc,
      cramers_v               = j$cv,
      df                      = df,
      ds                      = ds,
      phi_coefficient         = j$phi,
      pval_chisquare          = k$sig,
      pval_chisquare_lr       = m$sig_lr
    )
  }

  class(result) <- "infer_chisq_assoc_test"
  return(result)
}

#' @export
print.infer_chisq_assoc_test <- function(x, ...) {
  print_chisq_test(x)
}

# chi square association
df_chi <- function(twoway) {
  (nrow(twoway) - 1) * (ncol(twoway) - 1)
}

efmat <- function(twoway) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
  mat2 <- matrix(colSums(twoway), nrow = 1)

  mat1 %*% mat2
}

pear_chsq <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(stats::pchisq(chi, df, lower.tail = F), 4)

  list(chi = chi, sig = sig)
}

lr_chsq <- function(twoway, df, ef) {
  chilr  <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
  sig_lr <- round(stats::pchisq(chilr, df, lower.tail = F), 4)

  list(chilr = chilr, sig_lr = sig_lr)
}

lr_chsq2 <- function(twoway, df, ef, ds) {
  chilr  <- round(2 * sum(matrix(twoway, ncol = ds) %*% matrix(log(twoway / ef), nrow = ds)), 4)
  sig_lr <- round(stats::pchisq(chilr, df, lower.tail = F), 4)

  list(chilr = chilr, sig_lr = sig_lr)
}

yates_chsq <- function(twoway) {
  way2        <- twoway[, c(2, 1)]
  total       <- sum(twoway)
  prods       <- prod(diag(twoway)) - prod(diag(way2))
  prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
  chi_y       <- round((total * (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
  sig_y       <- round(stats::pchisq(chi_y, 1, lower.tail = F), 4)

  list(chi_y = chi_y, sig_y = sig_y, total = total, prod_totals = prod_totals)
}

mh_chsq <- function(twoway, total, prod_totals) {
  num    <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
  den    <- prod_totals / ((total ^ 3) - (total ^ 2))
  chimh  <- round((num ^ 2) / den, 4)
  sig_mh <- round(stats::pchisq(chimh, 1, lower.tail = F), 4)

  list(chimh = chimh, sig_mh = sig_mh)
}

efm <- function(twoway, dk) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = dk[1])
  mat2 <- matrix(colSums(twoway), ncol = dk[2])

  mat1 %*% mat2
}

pear_chi <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(stats::pchisq(chi, df, lower.tail = F), 4)

  list(chi = chi, sig = sig)
}

chigf <- function(x, y, chi) {
  twoway <- matrix(table(x, y),
    nrow = nlevels(as.factor(x)),
    ncol = nlevels(as.factor(y))
  )
  total <- sum(twoway)
  phi   <- round(sqrt(chi / total), 4)
  cc    <- round(sqrt(chi / (chi + total)), 4)
  q     <- min(nrow(twoway), ncol(twoway))
  cv    <- round(sqrt(chi / (total * (q - 1))), 4)

  list(phi = phi, cc = cc, cv = cv)
}
