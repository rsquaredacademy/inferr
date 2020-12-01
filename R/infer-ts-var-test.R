#' @title Two Sample Variance Comparison Test
#' @description  \code{infer_ts_var_test} performs tests on the equality of standard
#' deviations (variances).
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... numeric; column(s) in \code{data}
#' @param group_var factor; column in \code{data}
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter.
#' @return \code{infer_ts_var_test} returns an object of class \code{"infer_ts_var_test"}.
#' An object of class \code{"infer_ts_var_test"} is a list containing the
#' following components:
#'
#' \item{f}{f statistic}
#' \item{lower}{lower one-sided p-value}
#' \item{upper}{upper one-sided p-value}
#' \item{two_tail}{two-sided p-value}
#' \item{vars}{variances for each level of the grouping variable}
#' \item{avgs}{means for each level of the grouping variable}
#' \item{sds}{standard deviations for each level of the grouping variable}
#' \item{ses}{standard errors for each level of the grouping variable}
#' \item{avg}{combined mean}
#' \item{sd}{combined standard deviation}
#' \item{se}{estimated combined standard error}
#' \item{n1}{numerator degrees of freedom}
#' \item{n2}{denominator degrees of freedom}
#' \item{lens}{number of observations for each level of grouping variable}
#' \item{len}{number of observations}
#' \item{lev}{levels of the grouping variable}
#' \item{type}{alternative hypothesis}
#' @section Deprecated Function:
#' \code{var_test()} has been deprecated. Instead use \code{infer_ts_var_test()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{var.test}}
#' @examples
#' # using grouping variable
#' infer_ts_var_test(hsb, read, group_var = female, alternative = 'less')
#'
#' # using two variables
#' infer_ts_var_test(hsb, read, write, alternative = 'less')
#'
#' @export
#'
infer_ts_var_test <- function(data, ..., group_var = NULL,
                              alternative = c("less", "greater", "all")) UseMethod("infer_ts_var_test")

#' @export
#'
infer_ts_var_test.default <- function(data, ..., group_var = NULL,
                                      alternative = c("less", "greater", "all")) {

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

    out   <- gvar(ln, ly)
    fdata <- unlist(z)

    groupvars <-
      out %>%
      unlist() %>%
      as.factor()

    lev <- names(data[varyables])

  } else {

    fdata     <- fdata[[1]]
    groupvars <- data[[groupvar]]
    lev       <- levels(groupvars)

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }


  type <- match.arg(alternative)
  k    <- var_comp(fdata, groupvars)

  out <- list(avg   = k$avg,
              avgs  = k$avgs,
              f     = k$f,
              len   = k$len,
              lens  = k$lens,
              lev   = lev,
              lower = k$lower,
              n1    = k$n1,
              n2    = k$n2,
              sd    = k$sd,
              sds   = k$sds,
              se    = k$se,
              ses   = k$ses,
              type  = type,
              upper = k$upper,
              vars  = k$vars)

  class(out) <- "infer_ts_var_test"
  return(out)
}

#' @export
#'
print.infer_ts_var_test <- function(x, ...) {
  print_var_test(x)
}

var_comp <- function(variable, group_var) {

  comp  <- stats::complete.cases(variable, group_var)
  cvar  <- variable[comp]
  gvar  <- group_var[comp]

  d     <- data.frame(cvar, gvar)
  vals  <- tibble_stats(d, "cvar", "gvar")
  lass  <- tbl_stats(d, "cvar")

  lens  <- vals[[2]] %>% purrr::map_int(1)
  vars  <- vals[[4]] %>% purrr::map_dbl(1)

  f     <- vars[1] / vars[2]
  n1    <- lens[1] - 1
  n2    <- lens[2] - 1
  lower <- stats::pf(f, n1, n2)
  upper <- stats::pf(f, n1, n2, lower.tail = FALSE)

  list(avg   = round(lass[2], 2),
       avgs  = round((vals[[3]] %>% purrr::map_dbl(1)), 2),
       f     = round(f, 4),
       len   = lass[1],
       lens  = lens,
       lower = round(lower, 4),
       n1    = n1,
       n2    = n2,
       sd    = round(lass[3], 2),
       sds   = round((vals[[5]] %>% purrr::map_dbl(1)), 2),
       se    = round(lass[4], 2),
       ses   = round((vals[[6]] %>% purrr::map_dbl(1)), 2),
       upper = round(upper, 4),
       vars  = round(vars, 2))

}

tibble_stats <- function(data, x, y) {

  dat <- data.table(data[c(x, y)])

  out <- dat[, .(length = length(get(x)),
                     mean = mean(get(x)),
                     var = stats::var(get(x)),
                     sd = stats::sd(get(x))),
                  by = y]

  out[, ':='(ses = sd / sqrt(length))]
  setDF(out)
  out <- out[order(out[, 1]),]
  return(out)

}

tbl_stats <- function(data, y) {

  dat <- data[[y]]
  c(length(dat), mean(dat), sd(dat), (sd(dat) / sqrt(length(dat))))
             
}
