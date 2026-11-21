#' @title One Way ANOVA
#' @description One way analysis of variance
#' @param data a \code{data.frame} or a \code{tibble}
#' @param x numeric; column in \code{data}
#' @param y factor; column in \code{data}
#' @param ... additional arguments passed to or from other methods
#' @details Supported inputs: finite data with absolute value \code{<= 1e150}
#' and sample sizes \code{<= 1e15}. A response with standard deviation at most
#' \code{1e-12 * mean(abs(x))} is treated as numerically constant and rejected.
#' This tolerance is purely relative to the response's magnitude, with no
#' absolute minimum scale; the guard is computed from a scaled response so it
#' does not underflow or overflow.
#'
#' Sums of squares are computed from unrounded scaled values and converted back
#' to the response's original units before rounding. If a positive sum of
#' squares or mean square is too small or too large to represent as a double at
#' that response scale (for example an ordinary spread around \code{1e-170}),
#' the test stops with an error naming the response rather than returning zero
#' or infinity.
#' The sums of squares and mean squares are stored to three decimal places, as
#' is the F statistic. The p-value is computed from the unrounded F statistic;
#' an underflowed zero is printed as a bound such as \code{< 0.0001}.
#'
#' An exact fit (within-groups sum of squares zero with positive between-groups
#' sum of squares) is a boundary case, not an error: the F statistic is
#' \code{Inf} and the p-value is \code{0}. Only a response with no total
#' variation at all is undefined and stops with an error.
#' @return \code{ifr_oneway_anova} returns an object of class \code{"ifr_oneway_anova"}.
#' An object of class \code{"ifr_oneway_anova"} is a list containing the
#' following components:
#'
#' \item{adjusted_r2}{adjusted r squared value}
#' \item{df_btw}{between groups degrees of freedom}
#' \item{df_within}{within groups degrees of freedom}
#' \item{df_total}{total degrees of freedom}
#' \item{fstat}{f value}
#' \item{group_stats}{group statistics}
#' \item{ms_btw}{between groups mean square}
#' \item{ms_within}{within groups mean square}
#' \item{obs}{number of observations}
#' \item{pval}{p value; underflow is printed as a bound}
#' \item{pval_raw}{p value, retained unrounded}
#' \item{r2}{r squared value}
#' \item{rmse}{root mean squared error}
#' \item{ss_between}{between group sum of squares}
#' \item{ss_within}{within group sum of squares}
#' \item{ss_total}{total sum of squares}
#'
#' @section Deprecated Function:
#' \code{infer_oneway_anova()} has been deprecated. Instead use
#' \code{ifr_oneway_anova()}
#'
#' @references Kutner, M. H., Nachtsheim, C., Neter, J., & Li, W. (2005).
#' Applied linear statistical models. Boston: McGraw-Hill Irwin.
#'
#' @examples
#' ifr_oneway_anova(mtcars, mpg, cyl)
#' ifr_oneway_anova(hsb, write, prog)
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' @export
#'
ifr_oneway_anova <- function(data, x, y, ...) UseMethod("ifr_oneway_anova")

#' @export
ifr_oneway_anova.default <- function(data, x, y, ...) {

  x1 <- deparse(substitute(x))
  y1 <- deparse(substitute(y))

  fdata        <- data[c(x1, y1)]
  fdata        <- fdata[complete.cases(fdata), ]

  response     <- fdata[[x1]]

  if (length(response) < 2L) {
    stop(paste0("response '", x1,
                "' must contain at least two non-missing observations"),
         call. = FALSE)
  }

  check_finite_range(response, x1)
  check_max_sample_size(length(response), "sample size")

  # a constant response has no between- or within-group variation, so the F
  # ratio is 0/0; report the undefined test instead of a NaN with a spurious
  # R-squared produced by floating-point error in lm(). The guard is purely
  # relative to the size of the response, and is computed after scaling the
  # response so that tiny or large finite inputs cannot underflow or overflow.
  response_scale <- max(abs(response))

  if (!is.finite(response_scale) || response_scale == 0) {
    stop(paste0("response '", x1,
                "' has no variation; a one-way ANOVA is undefined"),
         call. = FALSE)
  }

  response_scaled <- response / response_scale
  response_sd     <- sqrt(mean((response_scaled - mean(response_scaled)) ^ 2))
  response_mean   <- mean(abs(response_scaled))

  check_finite_moment(response_sd, "standard deviation", x1)

  if (response_sd == 0 || response_sd <= 1e-12 * response_mean) {
    stop(paste0("response '", x1,
                "' has no variation; a one-way ANOVA is undefined"),
         call. = FALSE)
  }

  # keep ordinary inputs bit-for-bit unchanged; rescale only when the response
  # magnitude would make the sums of squares underflow or overflow
  work  <- fdata
  scale <- 1

  if (response_scale > 1e100 || response_scale < 1e-100) {
    scale <- response_scale
    work[[x1]] <- response / scale
  }

  sample_mean  <- anova_avg(work, x1)
  sample_stats <- anova_split(work, x1, y1, sample_mean)
  k            <- anova_calc(work, sample_stats, x1, y1)

  # an exact fit has zero within-groups variation and a positive between-groups
  # sum of squares: F is the boundary value Inf with p = 0, as in the earlier
  # release. Any other non-finite F remains an error.
  exact_fit <- k$mse == 0 && k$mstr > 0 && is.infinite(k$f) && k$f > 0
  if (!exact_fit) {
    check_finite_moment(k$f, "F statistic", x1)
  }
  check_finite_moment(k$sig, "p-value", x1)

  sample_stats[[3]] <- sample_stats[[3]] * scale
  sample_stats[[5]] <- sample_stats[[5]] * scale

  # convert the unrounded scaled sums of squares back to the original units,
  # then round once; rounding before this conversion loses the leading digits
  # at very large or very small response scales
  ss_between <- k$sstr * scale ^ 2
  ss_within  <- k$ssee * scale ^ 2
  ss_total   <- k$total * scale ^ 2
  ms_btw     <- k$mstr * scale ^ 2
  ms_within  <- k$mse * scale ^ 2

  # a positive scaled sum whose product underflows cannot be reported honestly
  if ((k$sstr > 0 && ss_between == 0) || (k$ssee > 0 && ss_within == 0) ||
      (k$mstr > 0 && ms_btw == 0) || (k$mse > 0 && ms_within == 0)) {
    stop(paste0("sums of squares for response '", x1, "' underflow at this ",
                "response scale; rescale '", x1, "' to a larger magnitude"),
         call. = FALSE)
  }

  if (any(!is.finite(c(ss_between, ss_within, ss_total,
                       ms_btw, ms_within)))) {
    stop(paste0("sums of squares for response '", x1, "' exceed the largest ",
                "representable double; rescale '", x1,
                "' to a smaller magnitude"),
         call. = FALSE)
  }

  result <-
    list(
      adjusted_r2 = round(k$reg$adj.r.squared, 4),
      df_btw      = k$df_sstr,
      df_total    = k$df_sst,
      df_within   = k$df_sse,
      fstat       = k$f,
      group_stats = sample_stats[, c(1, 2, 3, 5)],
      ms_btw      = round(ms_btw, 3),
      ms_within   = round(ms_within, 3),
      obs         = k$obs,
      pval        = k$sig,
      pval_raw    = k$sig,
      r2          = round(k$reg$r.squared, 4),
      rmse        = round(k$reg$sigma * scale, 4),
      ss_between  = round(ss_between, 3),
      ss_total    = round(ss_total, 3),
      ss_within   = round(ss_within, 3))

  class(result) <- "ifr_oneway_anova"
  return(result)
}

#' @export
#' @rdname ifr_oneway_anova
#' @usage NULL
#'
infer_oneway_anova <- function(data, x, y, ...) {
  .Deprecated("ifr_oneway_anova()")
}

#' @export
print.ifr_oneway_anova <- function(x, ...) {
  print_owanova(x)
}

#' @import magrittr
#' @importFrom stats var sd
#' @importFrom data.table data.table := setDF
anova_split <- function(data, x, y, sample_mean) {

  dat <- data.frame(.ifr_value = data[[x]], .ifr_group = data[[y]])
  dm  <- data.table(dat)

  by_factor <- dm[, .(length = .N,
                      mean   = mean(.ifr_value),
                      var    = var(.ifr_value),
                      sd     = sd(.ifr_value),
                      sse    = sum((.ifr_value - mean(.ifr_value)) ^ 2)),
                  by = ".ifr_group"]

  by_factor[, sst := length * ((mean - sample_mean) ^ 2)]

  setDF(by_factor)
  by_factor <- by_factor[order(by_factor[, 1]),]
  names(by_factor)[1] <- y

  return(by_factor)
}

anova_avg <- function(data, y) {

  mean(data[[y]])

}

#' @importFrom stats pf as.formula lm
anova_calc <- function(data, sample_stats, x, y) {

  var_names <- names(data[c(x, y)])

  sample_stats %>%
    use_series(sst) %>%
    sum() -> sstr_raw

  sample_stats %>%
    use_series(sse) %>%
    sum() -> ssee_raw

  df_sstr <- nrow(sample_stats) - 1
  df_sse  <- nrow(data) - nrow(sample_stats)
  df_sst  <- nrow(data) - 1

  if (df_sse < 1) {
    stop("ANOVA requires residual degrees of freedom.", call. = FALSE)
  }

  mstr    <- sstr_raw / df_sstr
  mse     <- ssee_raw / df_sse
  f       <- mstr / mse
  sig     <- pf(f, df_sstr, df_sse, lower.tail = FALSE)
  obs     <- nrow(data)
  regs    <- paste(var_names[1], "~ as.factor(", var_names[2], ")")
  model   <- lm(as.formula(regs), data = data)
  reg     <- summary(model)

  out <- list(
    sstr = sstr_raw, ssee = ssee_raw, total = sstr_raw + ssee_raw,
    df_sstr = df_sstr, df_sse = df_sse, df_sst = df_sst,
    mstr = mstr, mse = mse,
    f = round(f, 3), sig = sig, obs = obs, model = model, reg = reg
  )

  return(out)
}
