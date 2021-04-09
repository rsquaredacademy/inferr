#' @importFrom stats as.formula lm pf
#' @importFrom rlang enquo !!
#' @title One Way ANOVA
#' @description One way analysis of variance
#' @param data a \code{data.frame} or a \code{tibble}
#' @param x numeric; column in \code{data}
#' @param y factor; column in \code{data}
#' @param ... additional arguments passed to or from other methods
#' @return \code{owanova} returns an object of class \code{"owanova"}.
#' An object of class \code{"owanova"} is a list containing the
#' following components:
#'
#' \item{between}{between group sum of squares}
#' \item{within}{within group sum of squares}
#' \item{total}{total sum of squares}
#' \item{df_btw}{between groups degress of freedom}
#' \item{df_within}{within groups degress of freedom}
#' \item{df_total}{total degress of freedom}
#' \item{ms_btw}{between groups mean square}
#' \item{ms_within}{within groups mean square}
#' \item{f}{f value}
#' \item{p}{p value}
#' \item{r2}{r squared value}
#' \item{ar2}{adjusted r squared value}
#' \item{sigma}{root mean squared error}
#' \item{obs}{number of observations}
#' \item{tab}{group statistics}
#' @section Deprecated Functions:
#' \code{owanova()} has been deprecated. Instead use \code{infer_oneway_anova()}.
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
  x1 <- enquo(x)
  y1 <- enquo(y)

  fdata <-
    data %>%
    select(!! x1, !! y1)

  sample_mean <- anova_avg(fdata, !! x1)
  sample_stats <- anova_split(fdata, !! x1, !! y1, sample_mean)
  k <- anova_calc(fdata, sample_stats, !! x1, !! y1)


  result <- list(
    between = k$sstr, within = k$ssee, total = k$total,
    df_btw = k$df_sstr, df_within = k$df_sse,
    df_total = k$df_sst, ms_btw = k$mstr, ms_within = k$mse,
    f = k$f, p = k$sig, r2 = round(k$reg$r.squared, 4),
    ar2 = round(k$reg$adj.r.squared, 4),
    sigma = round(k$reg$sigma, 4), obs = k$obs,
    tab = sample_stats[, c(1, 2, 3, 5)]
  )

  class(result) <- "infer_oneway_anova"
  return(result)
}

#' @export
#' @rdname infer_oneway_anova
#' @usage NULL
#'
owanova <- function(data, x, y, ...) {
  .Deprecated("infer_oneway_anova()")
  infer_oneway_anova(data, x, y, ...)
}

#' @export
print.infer_oneway_anova <- function(x, ...) {
  print_owanova(x)
}