#' @importFrom stats anova model.frame formula
#' @importFrom purrr map_int
#' @importFrom rlang quo_is_null
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
#' {Brown, M. B., and A. B. Forsythe. 1974. Robust tests for the equality of variances. Journal of the American Statistical

#'


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
infer_levene_test.default <- function(data, ..., group_var = NULL,
                                      trim_mean = 0.1) {
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
  } else {
    fdata <-
      fdata %>%
      pull(1)

    groupvars <-
      data %>%
      pull(!! groupvar)

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }

  k <- lev_comp(fdata, groupvars, trim_mean)

  out <- list(
    bf = k$bf, p_bf = k$p_bf, lev = k$lev, p_lev = k$p_lev,
    bft = k$bft, p_bft = k$p_bft, avgs = k$avgs, sds = k$sds,
    avg = k$avg, sd = k$sd, n = k$n, levs = k$levs, n_df = k$n_df,
    d_df = k$d_df, lens = k$lens
  )

  class(out) <- "infer_levene_test"
  return(out)
}

#' @export
#' @rdname infer_levene_test
#' @usage NULL
#'
levene_test <- function(variable, ..., group_var = NULL,
                        trim.mean = 0.1) {
  .Deprecated("infer_levene_test()")
}

#' @export
#'
print.infer_levene_test <- function(x, ...) {
  print_levene_test(x)
}
