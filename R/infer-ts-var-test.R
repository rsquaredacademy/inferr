#' @importFrom stats complete.cases
#' @importFrom purrr map_dbl
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
  groupvar <- enquo(group_var)

  varyables <- quos(...)

  fdata <-
    data %>%
    select(!!! varyables)

  if (quo_is_null(groupvar)) {
    z <- as.list(fdata)
    ln <- z %>% map_int(length)
    ly <- seq_len(length(z))

    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out <- gvar(ln, ly)

    fdata <- unlist(z)

    groupvars <-
      out %>%
      unlist() %>%
      as.factor()

    lev <-
      data %>%
      select(!!! varyables) %>%
      names()
  } else {
    fdata <-
      fdata %>%
      pull(1)

    groupvars <-
      data %>%
      pull(!! groupvar)

    lev <- levels(groupvars)

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }


  type <- match.arg(alternative)
  k <- var_comp(fdata, groupvars)

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
#' @rdname infer_ts_var_test
#' @usage NULL
#'
var_test <- function(variable, ..., group_var = NA,
                     alternative = c("less", "greater", "all")) {
  .Deprecated("infer_ts_var_test()")
}

#' @export
#'
print.infer_ts_var_test <- function(x, ...) {
  print_var_test(x)
}