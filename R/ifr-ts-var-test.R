#' @title Two Sample Variance Comparison Test
#' @description  \code{ifr_ts_var_test} performs tests on the equality of standard
#' deviations (variances). Observations with missing values are removed.
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... numeric; column(s) in \code{data}
#' @param group_var factor or character; column in \code{data}
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "less" (default), "greater" or "all". You can specify
#' just the initial letter.
#' @details Supported inputs: finite data with absolute value \code{<= 1e150}
#' and sample sizes \code{<= 1e15}. Missing values are removed. Exactly two
#' variables, or one variable and a two-level grouping variable, are required.
#' A constant group is a boundary case, not an error: when exactly one group is
#' constant and the other has a positive, finite variance, the F statistic is
#' returned as \code{0} or \code{Inf} with matching tail probabilities, as in
#' \code{\link[stats]{var.test}}. Both groups constant is undefined and stops
#' with a clear error; so does a non-constant group whose variance is not
#' positive and finite (for example a variance too small to represent). The F
#' statistic is stored to four decimal places and unrounded in
#' \code{f_raw}; the variances and group statistics are rounded for display.
#' One-sided p-values are stored to four decimal places and unrounded in
#' \code{lower_raw} and \code{upper_raw}; an underflowed positive tail is
#' printed as a bound using \code{f_raw}, not the rounded statistic.
#'
#' @return \code{ifr_ts_var_test} returns an object of class \code{"ifr_ts_var_test"}.
#' An object of class \code{"ifr_ts_var_test"} is a list containing the
#' following components:
#'
#' \item{f}{f statistic, stored to four decimal places}
#' \item{f_raw}{unrounded f statistic}
#' \item{lower}{lower one-sided p-value, stored to four decimal places}
#' \item{lower_raw}{unrounded lower one-sided p-value}
#' \item{upper}{upper one-sided p-value, stored to four decimal places}
#' \item{upper_raw}{unrounded upper one-sided p-value}
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
#'
#' @section Deprecated Function:
#' \code{infer_ts_var_test()} has been deprecated. Instead use \code{ifr_ts_var_test()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' Snedecor, G. W. and Cochran, W. G. (1989). \emph{Statistical Methods}, 8th ed. Ames, IA: Iowa State University Press.
#'
#' @examples
#' # Do retail and hotel/restaurant/cafe customers vary equally in grocery spending?
#' # Use log(grocery) because spending is right-skewed.
#' w <- transform(wholesale_customers, log_grocery = log(grocery), log_milk = log(milk))
#'
#' # using grouping variable
#' ifr_ts_var_test(w, log_grocery, group_var = channel, alternative = 'all')
#'
#' # using two variables
#' # Do grocery and milk spending vary equally on the log scale?
#' ifr_ts_var_test(w, log_grocery, log_milk, alternative = 'all')
#'
#' @seealso \code{\link[stats]{var.test}}
#'
#' @export
#'
ifr_ts_var_test <- function(data, ..., group_var = NULL,
                              alternative = c("less", "greater", "all")) UseMethod("ifr_ts_var_test")

#' @export
#'
ifr_ts_var_test.default <- function(data, ..., group_var = NULL,
                                      alternative = c("less", "greater", "all")) {

  groupvar  <- deparse(substitute(group_var))
  varyables <- vapply(substitute(...()), deparse, NA_character_)
  fdata     <- data[varyables]

  if (groupvar == "NULL") {
    z  <- as.list(fdata)
    ln <- unlist(lapply(z, length))
    ly <- seq_len(length(z))

    if (length(z) != 2) {
      stop("Please specify exactly two variables.", call. = FALSE)
    }

    out       <- gvar(ln, ly)
    fdata     <- unlist(z)
    groupvars <- as.factor(unlist(out))
    lev       <- names(data[varyables])

  } else {

    if (length(varyables) != 1) {
      stop("Please specify exactly one variable when group_var is supplied.",
           call. = FALSE)
    }

    fdata     <- fdata[[1]]
    groupvars <- as.factor(data[[groupvar]])

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }

    keep      <- complete.cases(fdata, groupvars)
    fdata     <- fdata[keep]
    groupvars <- droplevels(as.factor(groupvars[keep]))
    lev       <- levels(groupvars)

    if (length(lev) != 2) {
      stop("Grouping variable must have exactly two levels.", call. = FALSE)
    }
  }


  type <- match.arg(alternative)
  k    <- var_comp(fdata, groupvars)

  out <- list(avg   = k$avg,
              avgs  = k$avgs,
              f     = k$f,
              f_raw = k$f_raw,
              len   = k$len,
              lens  = k$lens,
              lev   = lev,
              lower = k$lower,
              lower_raw = k$lower_raw,
              n1    = k$n1,
              n2    = k$n2,
              sd    = k$sd,
              sds   = k$sds,
              se    = k$se,
              ses   = k$ses,
              two_tail = k$two_tail,
              type  = type,
              upper = k$upper,
              upper_raw = k$upper_raw,
              vars  = k$vars)

  class(out) <- "ifr_ts_var_test"
  return(out)
}

#' @export
#' @rdname ifr_ts_var_test
#' @usage NULL
#'
infer_ts_var_test <- function(data, ..., group_var = NULL,
                              alternative = c("less", "greater", "all")) {
  .Deprecated("ifr_ts_var_test()")
}

#' @export
#'
print.ifr_ts_var_test <- function(x, ...) {
  print_var_test(x)
}

var_comp <- function(variable, group_var) {

  comp  <- complete.cases(variable, group_var)
  cvar  <- variable[comp]
  gvar  <- group_var[comp]
  check_finite_range(cvar, "variable")
  check_max_sample_size(length(cvar), "sample size")

  d     <- data.frame(cvar, gvar)
  vals  <- tibble_stats(d, "cvar", "gvar")
  lass  <- tbl_stats(d, "cvar")
  lens  <- vals[[2]]
  vars  <- vals[[4]]

  # a truly constant group is a legitimate boundary case when the other group
  # varies; only two constant groups (0/0) are undefined. A non-constant group
  # whose variance is zero, negative, or non-finite is not representable and
  # remains an error.
  constant <- unname(tapply(cvar, gvar, function(v) {
    length(v) >= 2L && all(v == v[1L])
  }))

  if (all(constant) ||
      any(!constant & (!is.finite(vars) | vars <= 0))) {
    stop(paste0("both groups must have positive, finite variances; the ",
                "variance ratio is undefined"),
         call. = FALSE)
  }

  f     <- vars[1] / vars[2]
  n1    <- lens[1] - 1
  n2    <- lens[2] - 1
  lower <- pf(f, n1, n2)
  upper <- pf(f, n1, n2, lower.tail = FALSE)
  two_tail <- 2 * min(lower, upper)

  list(avg      = round(lass[2], 2),
       avgs     = round(vals[[3]], 2),
       f        = round(f, 4),
       f_raw    = f,
       len      = lass[1],
       lens     = lens,
       lower    = round(lower, 4),
       lower_raw = lower,
       n1       = n1,
       n2       = n2,
       sd       = round(lass[3], 2),
       sds      = round(vals[[5]], 2),
       se       = round(lass[4], 2),
       ses      = round(vals[[6]], 2),
       two_tail = two_tail,
       upper    = round(upper, 4),
       upper_raw = upper,
       vars     = round(vars, 2))

}

tibble_stats <- function(data, x, y) {

  dat <- data.table(data[c(x, y)])

  out <- dat[, .(length = length(get(x)),
                 mean   = mean(get(x)),
                 var    = var(get(x)),
                 sd     = sd(get(x))),
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
