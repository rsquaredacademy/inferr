#' @title Two Independent Sample t Test
#' @description \code{ifr_ts_ind_ttest} compares the means of two independent groups in order to determine whether
#' there is statistical evidence that the associated population means are significantly different.
#' @param data a data frame
#' @param x factor; a column in \code{data}
#' @param y numeric; a column in \code{data}
#' @param confint confidence level
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#'
#' @return \code{ifr_ts_ind_ttest} returns an object of class \code{"ifr_ts_ind_ttest"}.
#' An object of class \code{"ifr_ts_ind_ttest"} is a list containing the
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
#'
#' @section Deprecated Function:
#' \code{infer_ts_ind_ttest()} has been deprecated. Instead use \code{ifr_ts_ind_ttest()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' # lower tail
#' ifr_ts_ind_ttest(hsb, female, write, alternative = 'less')
#'
#' # upper tail
#' ifr_ts_ind_ttest(hsb, female, write, alternative = 'greater')
#'
#' # both tails
#' ifr_ts_ind_ttest(hsb, female, write, alternative = 'both')
#'
#' # all tails
#' ifr_ts_ind_ttest(hsb, female, write, alternative = 'all')
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @export
#'
ifr_ts_ind_ttest <- function(data, x, y, confint = 0.95,
                               alternative = c("both", "less", "greater", "all"), ...) UseMethod("ifr_ts_ind_ttest")

#' @export
#'
ifr_ts_ind_ttest.default <- function(data, x, y, confint = 0.95,
                                       alternative = c("both", "less", "greater", "all"), ...) {

  x1   <- deparse(substitute(x))
  y1   <- deparse(substitute(y))
  yone <- names(data[y1])

  if (check_x(data, x1)) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  if (check_level(data, x1) > 2) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  method   <- match.arg(alternative)
  var_y    <- yone
  alpha    <- 1 - confint
  a        <- alpha / 2
  h        <- indth(data, x1, y1, a)
  grp_stat <- h
  g_stat   <- as.matrix(h)
  comb     <- indcomb(data, y1, a)
  k        <- indcomp(grp_stat, alpha)
  j        <- indsig(k$n1, k$n2, k$s1, k$s2, k$mean_diff)
  m        <- indpool(k$n1, k$n2, k$mean_diff, k$se_dif)

  result <- list(alternative      = method,
                 combined         = comb,
                 confint          = confint,
                 conf_diff        = round(k$conf_diff, 5),
                 den_df           = k$n2 - 1,
                 df_pooled        = m$df_pooled,
                 df_satterthwaite = j$d_f,
                 f                = round(k$s1 / k$s2, 4),
                 f_sig            = fsig(k$s1, k$s2, k$n1, k$n2),
                 levels           = g_stat[, 1],
                 lower            = g_stat[, 8],
                 mean             = g_stat[, 3],
                 mean_diff        = round(k$mean_diff, 3),
                 n                = k$n,
                 num_df           = k$n1 - 1,
                 obs              = g_stat[, 2],
                 sd               = g_stat[, 4],
                 sd_dif           = round(k$sd_dif, 3),
                 se               = g_stat[, 5],
                 se_dif           = round(k$se_dif, 3),
                 sig              = j$sig,
                 sig_l            = j$sig_l,
                 sig_pooled_l     = m$sig_pooled_l,
                 sig_pooled_u     = m$sig_pooled_u,
                 sig_pooled       = m$sig_pooled,
                 sig_u            = j$sig_u,
                 t_pooled         = round(m$t_pooled, 4),
                 t_satterthwaite  = round(j$t, 4),
                 upper            = g_stat[, 9],
                 var_y            = var_y)

  class(result) <- "ifr_ts_ind_ttest"
  return(result)

}

#' @export
#' @rdname ifr_ts_ind_ttest
#' @usage NULL
#'
infer_ts_ind_ttest <- function(data, x, y, confint = 0.95,
                               alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("ifr_ts_ind_ttest()")
}

#' @export
#'
print.ifr_ts_ind_ttest <- function(x, ...) {
  print_two_ttest(x)
}

indth <- function(data, x, y, a) {

  h       <- data_split(data, x, y)
  h$df    <- h$length - 1
  h$error <- qt(a, h$df) * -1
  h$lower <- h$mean_t - (h$error * h$std_err)
  h$upper <- h$mean_t + (h$error * h$std_err)

  return(h)
}

data_split <- function(data, x, y) {

  dat <- data.table(data[c(x, y)])
  out <- dat[, .(length  = length(get(y)),
                 mean_t  = mean_t(get(y)),
                 sd_t    = sd_t(get(y)),
                 std_err = std_err(get(y))),
            by = x]

  setDF(out)

}

indcomb <- function(data, y, a) {

  comb        <- da(data, y)
  comb$df     <- comb$length - 1
  comb$error  <- qt(a, comb$df) * -1
  comb$lower  <- round(comb$mean_t - (comb$error * comb$std_err), 5)
  comb$upper  <- round(comb$mean_t + (comb$error * comb$std_err), 5)
  names(comb) <- NULL

  return(comb)

}

da <- function(data, y) {

  dat <- data[[y]]
  data.frame(length  = length(dat),
             mean_t  = mean_t(dat),
             sd_t    = sd_t(dat),
             std_err = std_err(dat))

}

mean_t <- function(x) {
  round(mean(x), 3)
}

sd_t <- function(x) {
  round(sd(x), 3)
}

std_err <- function(x) {

  x %>%
    sd() %>%
    divide_by(x %>%
                length() %>%
                sqrt()) %>%
    round(3)

}

indcomp <- function(grp_stat, alpha) {

  n1        <- grp_stat[1, 2]
  n2        <- grp_stat[2, 2]
  n         <- n1 + n2
  means     <- grp_stat[, 3]
  mean_diff <- means[1] - means[2]
  sd1       <- grp_stat[1, 4]
  sd2       <- grp_stat[2, 4]
  s1        <- grp_stat[1, 4] ^ 2
  s2        <- grp_stat[2, 4] ^ 2
  sd_dif    <- sd_diff(n1, n2, s1, s2)
  se_dif    <- se_diff(n1, n2, s1, s2)
  conf_diff <- conf_int_p(mean_diff, se_dif, alpha = alpha)

  list(conf_diff = conf_diff,
       mean_diff = mean_diff,
       n         = n,
       n1        = n1,
       n2        = n2,
       s1        = s1,
       s2        = s2,
       sd1       = sd1,
       sd2       = sd2,
       sd_dif    = sd_dif,
       se_dif    = se_dif)

}

sd_diff <- function(n1, n2, s1, s2) {

  n1 <- n1 - 1
  n2 <- n2 - 1
  n  <- (n1 + n2) - 2

  (n1 * s1) %>%
    add(n2 * s2) %>%
    divide_by(n) %>%
    raise_to_power(0.5)

}

se_diff <- function(n1, n2, s1, s2) {

  df  <- n1 + n2 - 2
  n_1 <- n1 - 1
  n_2 <- n2 - 1

  (n_1 * s1) %>%
    add(n_2 * s2) %>%
    divide_by(df) -> v

  (1 / n1) %>%
    add(1 / n2) %>%
    multiply_by(v) %>%
    sqrt()

}

conf_int_p <- function(u, se, alpha = 0.05) {

  a     <- alpha / 2
  error <- round(qnorm(a), 3) * -1
  lower <- u - (error * se)
  upper <- u + (error * se)
  c(lower, upper)

}

indsig <- function(n1, n2, s1, s2, mean_diff) {

  d_f   <- as.vector(df(n1, n2, s1, s2))
  t     <- mean_diff / (((s1 / n1) + (s2 / n2)) ^ 0.5)
  sig_l <- round(pt(t, d_f), 4)
  sig_u <- round(pt(t, d_f, lower.tail = FALSE), 4)

  if (sig_l < 0.5) {
    sig <- round(pt(t, d_f) * 2, 4)
  } else {
    sig <- round(pt(t, d_f, lower.tail = FALSE) * 2, 4)
  }

  list(d_f   = d_f,
       sig_l = sig_l,
       sig_u = sig_u,
       sig   = sig,
       t     = t)

}

df <- function(n1, n2, s1, s2) {

  sn1 <- s1 / n1
  sn2 <- s2 / n2
  m1  <- 1 / (n1 - 1)
  m2  <- 1 / (n2 - 1)
  num <- (sn1 + sn2) ^ 2
  den <- (m1 * (sn1 ^ 2)) + (m2 * (sn2 ^ 2))

  round(num / den)

}

fsig <- function(s1, s2, n1, n2) {

  round(min(
    pf((s1 / s2), (n1 - 1), (n2 - 1)),
    pf((s1 / s2), (n1 - 1), (n2 - 1),
      lower.tail = FALSE
    )
  ) * 2, 4)

}


indpool <- function(n1, n2, mean_diff, se_dif) {

  df_pooled    <- (n1 + n2) - 2
  t_pooled     <- mean_diff / se_dif
  sig_pooled_l <- round(pt(t_pooled, df_pooled), 4)
  sig_pooled_u <- round(pt(t_pooled, df_pooled, lower.tail = FALSE), 4)

  if (sig_pooled_l < 0.5) {
    sig_pooled <- round(pt(t_pooled, df_pooled) * 2, 4)
  } else {
    sig_pooled <- round(pt(t_pooled, df_pooled, lower.tail = FALSE) * 2, 4)
  }

  list(df_pooled    = df_pooled,
       sig_pooled_l = sig_pooled_l,
       sig_pooled_u = sig_pooled_u,
       sig_pooled   = sig_pooled,
       t_pooled     = t_pooled)

}

check_x <- function(data, x) {

  !is.factor(data[[x]])

}

check_level <- function(data, x) {

  nlevels(data[[x]])

}
