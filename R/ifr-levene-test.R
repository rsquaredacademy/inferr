#' @title Levene's test for equality of variances
#' @description  \code{infer_levene_test} reports Levene's robust test statistic
#' for the equality of variances and the
#' two statistics proposed by Brown and Forsythe that replace the mean in
#' Levene's formula with alternative location estimators. The first alternative
#' replaces the mean with the median. The second alternative replaces
#' the mean with the 10% trimmed mean.
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... numeric; columns in \code{data}
#' @param group_var factor; column in \code{data}
#' @param trim_mean trimmed mean
#' @return \code{infer_levene_test} returns an object of class \code{"infer_levene_test"}.
#' An object of class \code{"infer_levene_test"} is a list containing the
#' following components:
#'
#' \item{bf}{Brown and Forsythe f statistic}
#' \item{p_bf}{p-value for Brown and Forsythe f statistic}
#' \item{lev}{Levene's f statistic}
#' \item{p_lev}{p-value for Levene's f statistic}
#' \item{bft}{Brown and Forsythe f statistic using trimmed mean}
#' \item{p_bft}{p-value for Brown and Forsythe f statistic using trimmed mean}
#' \item{avgs}{mean for each level of the grouping variable}
#' \item{sds}{standard deviations for each level of the grouping variable}
#' \item{avg}{combined mean}
#' \item{sd}{combined standard deviation}
#' \item{n}{number of observations}
#' \item{n_df}{numerator degrees of freedom}
#' \item{d_df}{denominator degrees of freedom}
#' \item{levs}{levels of the grouping variable}
#' \item{lens}{number of observations for each level of the grouping variable}
#' \item{type}{alternative hypothesis}
#' @section Deprecated Function:
#' \code{levene_test()} has been deprecated. Instead use \code{infer_levene_test()}.
#' @references
#' {Bland, M. 2000. An Introduction to Medical Statistics. 3rd ed. Oxford: Oxford University Press.}
#'
#' {Brown, M. B., and A. B. Forsythe. 1974. Robust tests for the equality of variances. Journal of the American Statistical Association 69: 364–367.}
#'
#' {Carroll, R. J., and H. Schneider. 1985. A note on Levene’s tests for equality of variances. Statistics and Probability Letters 3: 191–194.}
#' @examples
#' # using grouping variable
#' infer_levene_test(hsb, read, group_var = race)
#'
#' # using  variables
#' infer_levene_test(hsb, read, write, socst)
#'
#' @export
#'
infer_levene_test <- function(data, ...) UseMethod("infer_levene_test")

#' @export
#' @rdname infer_levene_test
infer_levene_test.default <- function(data, ..., group_var = NULL, trim_mean = 0.1) {

  groupvar  <- deparse(substitute(group_var))
  varyables <- vapply(substitute(...()), deparse, NA_character_)
  fdata     <- data[varyables]

  if (groupvar == "NULL") {
    z  <- as.list(fdata)
    ln <- unlist(lapply(z, length))
    ly <- seq_len(length(z))

    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out       <- gvar(ln, ly)
    fdata     <- unlist(z)
    groupvars <- as.factor(unlist(out))

  } else {

    fdata     <- fdata[[1]]
    groupvars <- data[[groupvar]]

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }

  k <- lev_comp(fdata, groupvars, trim_mean)

  out <-
    list(avg   = k$avg,
         avgs  = k$avgs,
         bf    = k$bf,
         bft   = k$bft,
         d_df  = k$d_df,
         lens  = k$lens,
         lev   = k$lev,
         levs  = k$levs,
         n     = k$n,
         n_df  = k$n_df,
         p_bf  = k$p_bf,
         p_bft = k$p_bft,
         p_lev = k$p_lev,
         sd    = k$sd,
         sds   = k$sds)

  class(out) <- "infer_levene_test"
  return(out)
}


#' @export
#'
print.infer_levene_test <- function(x, ...) {
  print_levene_test(x)
}

#' @importFrom stats anova
lev_metric <- function(cvar, gvar, loc, ...) {

  metric <- tapply(cvar, gvar, loc, ...)
  y      <- abs(cvar - metric[gvar])
  result <- anova(lm(y ~ gvar))

  list(
    fstat = result$`F value`[1],
    p     = result$`Pr(>F)`[1]
  )

}

#' @importFrom stats complete.cases median
lev_comp <- function(variable, group_var, trim.mean) {

  comp <- complete.cases(variable, group_var)
  n    <- length(comp)
  k    <- nlevels(group_var)
  cvar <- variable[comp]
  gvar <- group_var[comp]
  lens <- tapply(cvar, gvar, length)
  avgs <- tapply(cvar, gvar, mean)
  sds  <- tapply(cvar, gvar, sd)
  bf   <- lev_metric(cvar, gvar, mean)
  lev  <- lev_metric(cvar, gvar, median)
  bft  <- lev_metric(cvar, gvar, mean, trim = trim.mean)

  list(
    avg   = round(mean(cvar), 2),
    avgs  = round(avgs, 2),
    bf    = round(bf$fstat, 4),
    bft   = round(bft$fstat, 4),
    d_df  = (n - k),
    lens  = lens,
    lev   = round(lev$fstat, 4),
    levs  = levels(gvar),
    n     = n,
    n_df  = (k - 1),
    p_bf  = round(bf$p, 4),
    p_bft = round(bft$p, 4),
    p_lev = round(lev$p, 4),
    sd    = round(sd(cvar), 2),
    sds   = round(sds, 2))

}
