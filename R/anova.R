#' @importFrom stats as.formula lm pf
#' @title One Way ANOVA
#' @description One way analysis of variance
#' @param data a data frame
#' @param x character vector; name of a continuous variable from \code{data}
#' @param y character vector; name of a categorical variable from \code{data}
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
#' @references Kutner, M. H., Nachtsheim, C., Neter, J., & Li, W. (2005).
#' Applied linear statistical models. Boston: McGraw-Hill Irwin.
#'
#' @seealso \code{\link[stats]{anova}}
#' @examples
#' owanova(mtcars, 'mpg', 'cyl')
#' owanova(hsb, 'write', 'prog')
#' @export
#'
owanova <- function(data, x, y, ...) UseMethod('owanova')

#' @export
owanova.default <- function(data, x, y, ...) {

		if (!is.data.frame(data)) {
      stop('data must be a data frame')
    }

    if (!x %in% colnames(data)) {
      stop('x must be a column in data')
    }

    if (!y %in% colnames(data)) {
      stop('y must be a column in data')
    }

	  sample_mean <- anova_avg(data, x)
	  sample_stats <- anova_split(data, x, y, sample_mean)

	   sstr <- round(sum(sample_stats$sst), 3)
	   ssee <- round(sum(sample_stats$sse), 3)
	  total <- round(sstr + ssee, 3)
	df_sstr <- nrow(sample_stats) - 1
	 df_sse <- nrow(data) - nrow(sample_stats)
	 df_sst <- nrow(data) - 1
	   mstr <- round(sstr / df_sstr, 3)
	    mse <- round(ssee / df_sse, 3)
	      f <- round(mstr / mse, 3)
	    sig <- round(1- pf(f, df_sstr, df_sse), 3)
	    obs <- nrow(data)
	   regs <- paste(x, '~ as.factor(', y, ')')
	  model <- lm(as.formula(regs), data = data)
	    reg <- summary(model)

	 result <- list( between = sstr,
								    within = ssee,
		       			     total = total,
								    df_btw = df_sstr,
								 df_within = df_sse,
								  df_total = df_sst,
								    ms_btw = mstr,
								 ms_within = mse,
								         f = f,
								         p = sig,
								        r2 = round(reg$r.squared, 4),
								       ar2 = round(reg$adj.r.squared, 4),
								     sigma = round(reg$sigma, 4),
								       obs = obs,
								       tab = format(sample_stats[, c(1, 2, 3, 5)], nsmall = 3))

	class(result) <- 'owanova'
	return(result)

}

#' @export
print.owanova <- function(x, ...) {
  print_owanova(x)
}
