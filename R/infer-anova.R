#' @title One Way ANOVA
#' @description One way analysis of variance
#' @param data a \code{data.frame} or a \code{tibble}
#' @param x numeric; column in \code{data}
#' @param y factor; column in \code{data}
#' @param ... additional arguments passed to or from other methods
#' @return \code{infer_oneway_anova} returns an object of class \code{"infer_oneway_anova"}.
#' An object of class \code{"infer_oneway_anova"} is a list containing the
#' following components:
#'
#' \item{adjusted_r2}{adjusted r squared value}
#' \item{df_btw}{between groups degress of freedom}
#' \item{df_within}{within groups degress of freedom}
#' \item{df_total}{total degress of freedom}
#' \item{fstat}{f value}
#' \item{group_stats}{group statistics}
#' \item{ms_btw}{between groups mean square}
#' \item{ms_within}{within groups mean square}
#' \item{obs}{number of observations}
#' \item{pval}{p value}
#' \item{r2}{r squared value}
#' \item{rmse}{root mean squared error}
#' \item{ss_between}{between group sum of squares}
#' \item{ss_within}{within group sum of squares}
#' \item{ss_total}{total sum of squares}
#'
#' @references Kutner, M. H., Nachtsheim, C., Neter, J., & Li, W. (2005).
#' Applied linear statistical models. Boston: McGraw-Hill Irwin.
#'
#' @seealso \code{\link[stats]{anova}}
#' @examples
#' infer_oneway_anova(mtcars, mpg, cyl)
#' infer_oneway_anova(hsb, write, prog)
#' @export
#'
infer_oneway_anova <- function(data, x, y, ...) UseMethod("infer_oneway_anova")

#' @export
infer_oneway_anova.default <- function(data, x, y, ...) {

  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))

  fdata        <- data[c(x1, y1)]
  sample_mean  <- anova_avg(fdata, x1)
  sample_stats <- anova_split(fdata, x1, y1, sample_mean)
  k            <- anova_calc(fdata, sample_stats, x1, y1)


  result <-
    list(
      adjusted_r2 = round(k$reg$adj.r.squared, 4),
      df_btw      = k$df_sstr,
      df_total    = k$df_sst,
      df_within   = k$df_sse,
      fstat       = k$f,
      group_stats = sample_stats[, c(1, 2, 3, 5)],
      ms_btw      = k$mstr,
      ms_within   = k$mse,
      obs         = k$obs,
      pval        = k$sig,
      r2          = round(k$reg$r.squared, 4),
      rmse        = round(k$reg$sigma, 4),
      ss_between  = k$sstr,
      ss_total    = k$total,
      ss_within   = k$ssee)

  class(result) <- "infer_oneway_anova"
  return(result)
}

#' @export
print.infer_oneway_anova <- function(x, ...) {
  print_owanova(x)
}

#' @import magrittr
#' @importFrom data.table data.table := setDF
anova_split <- function(data, x, y, sample_mean) {

  dat <- data[c(y, x)]
  dm  <- data.table(dat)

  by_factor <- dm[, .(length = length(get(x)),
                     mean = mean(get(x)),
                     var = stats::var(get(x)),
                     sd = stats::sd(get(x))),
                  by = y]

  by_factor[, ':='(sst = length * ((mean - sample_mean) ^ 2),
                   sse = (length - 1) * var)]

  setDF(by_factor)
  by_factor <- by_factor[order(by_factor[, 1]),]

  return(by_factor)
}

anova_avg <- function(data, y) {

  mean(data[[y]])

}

anova_calc <- function(data, sample_stats, x, y) {

  var_names <- names(data[c(x, y)])

  sstr <-
    sample_stats %>%
    magrittr::use_series(sst) %>%
    sum() %>%
    round(3)

  ssee <-
    sample_stats %>%
    magrittr::use_series(sse) %>%
    sum() %>%
    round(3)

  total   <- round(sstr + ssee, 3)
  df_sstr <- nrow(sample_stats) - 1
  df_sse  <- nrow(data) - nrow(sample_stats)
  df_sst  <- nrow(data) - 1
  mstr    <- round(sstr / df_sstr, 3)
  mse     <- round(ssee / df_sse, 3)
  f       <- round(mstr / mse, 3)
  sig     <- round(1 - stats::pf(f, df_sstr, df_sse), 3)
  obs     <- nrow(data)
  regs    <- paste(var_names[1], "~ as.factor(", var_names[2], ")")
  model   <- stats::lm(stats::as.formula(regs), data = data)
  reg     <- summary(model)

  out <- list(
    sstr = sstr, ssee = ssee, total = total, df_sstr = df_sstr,
    df_sse = df_sse, df_sst = df_sst, mstr = mstr, mse = mse, f = f,
    sig = sig, obs = obs, model = model, reg = reg
  )

  return(out)
}
