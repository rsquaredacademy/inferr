#' @title Two Independent Sample t Test
#' @description \code{infer_ts_ind_ttest} compares the means of two independent groups in order to determine whether
#' there is statistical evidence that the associated population means are significantly different.
#' @param data a data frame
#' @param x factor; a column in \code{data}
#' @param y numeric; a column in \code{data}
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return \code{infer_ts_ind_ttest} returns an object of class \code{"infer_ts_ind_ttest"}.
#' An object of class \code{"infer_ts_ind_ttest"} is a list containing the
#' following components:
#'
#' \item{levels}{levels of \code{x}}
#' \item{obs}{number of observations of \code{y} for each level of \code{x}}
#' \item{n}{total number of observations}
#' \item{mean}{mean of \code{y} for each level of \code{x}}
#' \item{sd}{standard deviation of \code{y} for each level of \code{x}}
#' \item{se}{estimate of standard error of \code{y} for each level of \code{x}}
#' \item{lower}{lower limit for the mean of \code{y} for each level of \code{x}}
#' \item{upper}{upper limit for the mean of \code{y} for each level of \code{x}}
#' \item{combined}{a data frame; mean, standard deviation, standard error and
#' confidence limit of mean of \code{y}}
#' \item{mean_diff}{difference in mean of \code{y} for the two groups of \code{x}}
#' \item{se_dif}{estimate of the standard error for difference in mean of
#' \code{y} for the two groups of \code{x}} \item{sd_dif}{degrees of freedom}
#' \item{conf_diff}{confidence interval for \code{mean_diff}}
#' \item{df_pooled}{degrees of freedom for the pooled method}
#' \item{df_satterthwaite}{degrees of freedom for the Satterthwaite method}
#' \item{t_pooled}{t statistic for the pooled method}
#' \item{t_satterthwaite}{t statistic for the Satterthwaite method}
#' \item{sig_pooled}{two-sided p-value for the pooled method}
#' \item{sig_pooled_l}{lower one-sided p-value for the pooled method}
#' \item{sig_pooled_u}{upper one-sided p-value for the pooled method}
#' \item{sig}{two-sided p-value for the Satterthwaite method}
#' \item{sig_l}{lower one-sided p-value for the Satterthwaite method}
#' \item{sig_u}{upper one-sided p-value for the Satterthwaite method}
#' \item{num_df}{numerator degrees of freedom for folded f test}
#' \item{den_df}{denominator degrees of freedom for folded f test}
#' \item{f}{f value for the equality of variances test}
#' \item{f_sig}{p-value for the folded f test}
#' \item{var_y}{name of \code{y}}
#' \item{confint}{confidence level}
#' \item{alternative}{alternative hypothesis}
#' @section Deprecated Function:
#' \code{ind_ttest()} has been deprecated. Instead use \code{infer_ts_ind_ttest()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{t.test}}
#' @examples
#' # lower tail
#' infer_ts_ind_ttest(hsb, female, write, alternative = 'less')
#'
#' # upper tail
#' infer_ts_ind_ttest(hsb, female, write, alternative = 'greater')
#'
#' # both tails
#' infer_ts_ind_ttest(hsb, female, write, alternative = 'both')
#'
#' # all tails
#' infer_ts_ind_ttest(hsb, female, write, alternative = 'all')
#' @export
#'
infer_ts_ind_ttest <- function(data, x, y, confint = 0.95,
                               alternative = c("both", "less", "greater", "all"), ...) UseMethod("infer_ts_ind_ttest")

#' @export
#'
infer_ts_ind_ttest.default <- function(data, x, y, confint = 0.95,
                                       alternative = c("both", "less", "greater", "all"), ...) {
  x1 <- rlang::enquo(x)
  y1 <- rlang::enquo(y)

  yone <-
    data %>%
    dplyr::select(!! y1) %>%
    names()

  if (check_x(data, !! x1)) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  if (check_level(data, !! x1) > 2) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  method   <- match.arg(alternative)
  var_y    <- yone
  alpha    <- 1 - confint
  a        <- alpha / 2
  h        <- indth(data, !! x1, !! y1, a)
  grp_stat <- h
  g_stat   <- as.matrix(h)
  comb     <- indcomb(data, !! y1, a)
  k        <- indcomp(grp_stat, alpha)
  j        <- indsig(k$n1, k$n2, k$s1, k$s2, k$mean_diff)
  m        <- indpool(k$n1, k$n2, k$mean_diff, k$se_dif)

  result <- list(
    levels = g_stat[, 1], obs = g_stat[, 2], n = k$n,
    mean = g_stat[, 3], sd = g_stat[, 4], se = g_stat[, 5],
    lower = g_stat[, 8], upper = g_stat[, 9], combined = comb,
    mean_diff = round(k$mean_diff, 3), sd_dif = round(k$sd_dif, 3),
    se_dif = round(k$se_dif, 3),
    conf_diff = round(k$conf_diff, 5), df_pooled = m$df_pooled,
    df_satterthwaite = j$d_f, t_pooled = round(m$t_pooled, 4),
    t_satterthwaite = round(j$t, 4),
    sig_pooled_l = m$sig_pooled_l, sig_pooled_u = m$sig_pooled_u,
    sig_pooled = m$sig_pooled, sig = j$sig, sig_l = j$sig_l, sig_u = j$sig_u,
    num_df = k$n1 - 1, den_df = k$n2 - 1, f = round(k$s1 / k$s2, 4),
    f_sig = fsig(k$s1, k$s2, k$n1, k$n2), var_y = var_y, confint = confint,
    alternative = method
  )

  class(result) <- "infer_ts_ind_ttest"
  return(result)
}

#' @export
#'
print.infer_ts_ind_ttest <- function(x, ...) {
  print_two_ttest(x)
}
