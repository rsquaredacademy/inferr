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
  groupvar  <- rlang::enquo(group_var)
  varyables <- rlang::quos(...)
  fdata     <- dplyr::select(data, !!! varyables)

  if (rlang::quo_is_null(groupvar)) {
    z  <- as.list(fdata)
    ln <- z %>% purrr::map_int(length)
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

    lev <-
      data %>%
      dplyr::select(!!! varyables) %>%
      names()

  } else {

    fdata     <- dplyr::pull(fdata, 1)
    groupvars <- dplyr::pull(data, !! groupvar)
    lev       <- levels(groupvars)

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }


  type <- match.arg(alternative)
  k    <- var_comp(fdata, groupvars)

  out <- list(
    f = k$f, lower = k$lower, upper = k$upper, vars = k$vars,
    avgs = k$avgs, sds = k$sds, ses = k$ses, avg = k$avg, sd = k$sd,
    se = k$se, n1 = k$n1, n2 = k$n2, lens = k$lens, len = k$len,
    lev = lev, type = type
  )

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

  d     <- tibble::tibble(cvar, gvar)
  vals  <- tibble_stats(d, "cvar", "gvar")
  lass  <- tbl_stats(d, "cvar")

  lens  <- vals[[2]] %>% purrr::map_int(1)
  vars  <- vals[[4]] %>% purrr::map_dbl(1)

  f     <- vars[1] / vars[2]
  n1    <- lens[1] - 1
  n2    <- lens[2] - 1
  lower <- stats::pf(f, n1, n2)
  upper <- stats::pf(f, n1, n2, lower.tail = FALSE)

  list(
    f = round(f, 4), lower = round(lower, 4),
    upper = round(upper, 4),
    vars = round(vars, 2),
    avgs = round((vals[[3]] %>% purrr::map_dbl(1)), 2),
    sds = round((vals[[5]] %>% purrr::map_dbl(1)), 2),
    ses = round((vals[[6]] %>% purrr::map_dbl(1)), 2),
    avg = round(lass[2], 2),
    sd = round(lass[3], 2),
    se = round(lass[4], 2),
    n1 = n1,
    n2 = n2,
    lens = lens,
    len = lass[1]
  )

}

tibble_stats <- function(data, x, y) {

  by_factor <- data %>%
    dplyr::group_by(!! rlang::sym(y)) %>%
    dplyr::select(!! rlang::sym(y), !! rlang::sym(x)) %>%
    dplyr::summarise_all(dplyr::funs(length, mean, var = stats::var, sd = stats::sd)) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(
      ses = sd / sqrt(length)
    )

  return(by_factor)

}

tbl_stats <- function(data, y) {

  avg <- 
    data %>%
    dplyr::select(y) %>%
    dplyr::summarise_all(dplyr::funs(length, mean, sd = stats::sd)) %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(
      se = sd / sqrt(length)
    )

  return(unlist(avg, use.names = FALSE))

}