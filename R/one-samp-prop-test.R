#' @importFrom stats pnorm
#' @title One sample test of proportion
#' @description  \code{prop_test} performs tests on the equality of proportions
#' using large-sample statistics.
#' @param n number of observations
#' @param phat observed proportion
#' @param prob hypothesised proportion
#' @param data a categorical variable
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter.
#' @return \code{prop_test} returns an object of class \code{"prop_test"}.
#' An object of class \code{"prop_test"} is a list containing the
#' following components:
#'
#' \item{n}{number of observations}
#' \item{phat}{proportion of 1's}
#' \item{p}{assumed probability of success}
#' \item{z}{z statistic}
#' \item{sig}{p-value for z statistic}
#' \item{alt}{alternative hypothesis}
#' \item{obs}{observed number of 0's and 1's}
#' \item{exp}{expected number of 0's and 1's}
#' \item{deviation}{deviation of observed from expected}
#' \item{std}{standardized resiudals}
#'
#' @examples
#' # prop_test(200, 0.3, prob = 0.5)
#'
#' # using data set
#' prop_test(as.factor(hsb$female), prob = 0.5)
#' @export
#'
prop_test <- function(n, phat, prob = 0.5,
                      alternative = c('both', 'less', 'greater', 'all')) UseMethod('prop_test')

#' @export
#'
prop_test.default <- function(n, phat, prob = 0.5, conf.int = 0.95,
                      alternative = c('both', 'less', 'greater', 'all')) {


	n    <- n
	phat <- phat
	p    <- prob
	q    <- 1 - p
	obs  <- c(n * (1 - phat), n * phat)
	exp  <- n * c(q, p)
	dif  <- obs - exp
  dev  <- round((dif / exp) * 100, 2)
  std  <- round(dif / sqrt(exp), 2)
	num  <- phat - prob
	den  <- sqrt((p * q) / n)
	z    <- round(num / den, 4)
	lt   <- round(pnorm(z), 4)
	ut   <- round(1 - pnorm(z), 4)
	tt   <- round((1 - pnorm(z)) * 2, 4)
	alt  <- match.arg(alternative)

    if (alt == "all") {
        sig = c('two-both' = tt, 'less' = lt, 'greater' = ut)
    } else if (alt == "greater") {
        sig = ut
    } else if (alt == "less"){
        sig = lt
    } else {
        sig = tt
    }

    result <- list(n = n,
                   phat = phat,
                   p = prob,
                   z = z,
                   sig =
                   sig,
                   alt = alt,
                   obs = obs,
    	             exp = exp,
                   deviation = format(dev, nsmall = 2),
                   std = format(std, nsmall = 2))

    class(result) <- 'prop_test'
    return(result)

}

#' @export
#'
print.prop_test <- function(x, ...) {
  print_prop_test(x)
}


#' @export
#' @rdname prop_test
#'
prop_test.factor <- function(data, prob = 0.5, alternative = c('both', 'less', 'greater', 'all')) {

	if (!is.factor(data)) {
		data <- as.factor(data)
	}

  if (!is.numeric(prob)) {
    stop('prob must be numeric')
  }

  if((prob < 0) | (prob > 1)) {
    stop('prob must be between 0 and 1')
  }

  if (!is.numeric(conf.int)) {
    stop('conf.int must be numeric')
  }

	if (nlevels(data) > 2) {
		stop('Please specify a categorical variable with only 2 levels.')
	}

  n <- length(data)
	n1 <- table(data)[[2]]
	phat <- round(n1 / n, 4)
  prob <- prob
  alternative <- alternative

  prop_test.default(n, phat, prob, alternative)

}
