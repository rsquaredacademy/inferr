#' @title Two Sample Test of Proportion
#' @description Tests on the equality of proportions using
#' large-sample statistics. It tests that a sample has the same proportion
#' within two independent groups or two samples have the same proportion.
#' @param data a \code{data.frame} or \code{tibble}
#' @param var1 factor; column in \code{data}
#' @param var2 factor; column in \code{data}
#' @param var factor; column in \code{data}
#' @param group factor; column in \code{data}
#' @param n1 sample 1 size
#' @param n2 sample 2 size
#' @param p1 sample 1 proportion
#' @param p2 sample 2 proportion
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "both" (default), "greater", "less" or "all". You can specify
#' just the initial letter
#' @param ... additional arguments passed to or from other methods
#' @return an object of class \code{"infer_ts_prop_test"}.
#' An object of class \code{"infer_ts_prop_test"} is a list containing the
#' following components:
#'
#' \item{n1}{sample 1 size}
#' \item{n2}{sample 2 size}
#' \item{phat1}{sample 1 proportion}
#' \item{phat2}{sample 2 proportion}
#' \item{z}{z statistic}
#' \item{sig}{p-value for z statistic}
#' \item{alt}{alternative hypothesis}
#' @section Deprecated Functions:
#' \code{ts_prop_test()}, \code{ts_prop_grp()} and \code{ts_prop_calc()} have
#' been deprecated. Instead use \code{infer_ts_prop_test()},
#' \code{infer_ts_prop_grp()} and \code{infer_ts_prop_calc()}.
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#' @seealso \code{\link[stats]{prop.test}}
#' @examples
#' # using variables
#' # lower tail
#' infer_ts_prop_test(treatment, treatment1, treatment2,
#' alternative = 'less')
#'
#' # using groups
#' # lower tail
#' infer_ts_prop_grp(treatment2, outcome, female,
#' alternative = 'less')
#'
#' # using sample size and proportions
#' # lower tail
#' infer_ts_prop_calc(n1 = 30, n2 = 25, p1 = 0.3, p2 = 0.5, alternative = 'less')
#'
#' @export
#'
infer_ts_prop_test <- function(data, var1, var2,
                               alternative = c("both", "less", "greater", "all"), ...)
  UseMethod("infer_ts_prop_test")

#' @export
#'
infer_ts_prop_test.default <- function(data, var1, var2,
                                       alternative = c("both", "less", "greater", "all"), ...) {
  var_1 <- enquo(var1)
  var_2 <- enquo(var2)

  varone <-
    data %>%
    pull(!! var_1)

  vartwo <-
    data %>%
    pull(!! var_2)

  alt <- match.arg(alternative)
  k <- prop_comp2(varone, vartwo, alt)

  result <- list(
    n1 = k$n1, n2 = k$n2, phat1 = k$phat1, phat2 = k$phat2,
    z = k$z, sig = k$sig, alt = alt
  )

  class(result) <- "infer_ts_prop_test"
  return(result)
}

#' @export
#' @rdname infer_ts_prop_test
#' @usage NULL
#'
ts_prop_test <- function(var1, var2,
                         alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("infer_ts_prop_test()")
}

#' @export
#'
print.infer_ts_prop_test <- function(x, ...) {
  print_ts_prop_test(x)
}


#' @export
#' @rdname infer_ts_prop_test
#'
infer_ts_prop_grp <- function(data, var, group,
                              alternative = c("both", "less", "greater", "all")) {
  var1 <- enquo(var)
  group1 <- enquo(group)

  varone <-
    data %>%
    pull(!! var1)

  groupone <-
    data %>%
    pull(!! group1)

  if (nlevels(groupone) > 2) {
    stop("Grouping variable must be a binary factor variables.", call. = FALSE)
  }

  n <- tapply(varone, groupone, length)
  n1 <- n[[1]]
  n2 <- n[[2]]
  y <- tapply(varone, groupone, table)
  y1 <- y[[1]][[2]]
  y2 <- y[[2]][[2]]
  phat1 <- y1 / n1
  phat2 <- y2 / n2
  phat <- sum(y1, y2) / sum(n1, n2)
  num <- (phat1 - phat2)
  den1 <- phat * (1 - phat)
  den2 <- (1 / n1) + (1 / n2)
  den <- sqrt(den1 * den2)
  z <- num / den


  lt <- pnorm(z)
  ut <- round(pnorm(z, lower.tail = FALSE), 4)
  tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)

  alt <- match.arg(alternative)

  if (alt == "all") {
    sig <- c("both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <- list(
    n1 = n1,
    n2 = n2,
    phat1 = phat1,
    phat2 = phat2,
    z = round(z, 3),
    sig = round(sig, 3),
    alt = alt
  )

  class(out) <- "infer_ts_prop_test"
  return(out)
}

#' @export
#' @rdname infer_ts_prop_test
#' @usage NULL
#'
ts_prop_grp <- function(var, group,
                        alternative = c("both", "less", "greater", "all")) {
  .Deprecated("infer_ts_prop_grp()")
}


#' @export
#' @rdname infer_ts_prop_test
#'
infer_ts_prop_calc <- function(n1, n2, p1, p2,
                               alternative = c("both", "less", "greater", "all"), ...) {
  n1 <- n1
  n2 <- n2
  phat1 <- p1
  phat2 <- p2
  phat <- sum(n1 * p1, n2 * p2) / sum(n1, n2)
  num <- (phat1 - phat2)
  den1 <- phat * (1 - phat)
  den2 <- (1 / n1) + (1 / n2)
  den <- sqrt(den1 * den2)
  z <- num / den

  lt <- pnorm(z)
  ut <- round(pnorm(z, lower.tail = FALSE), 4)
  tt <- round(pnorm(abs(z), lower.tail = FALSE) * 2, 4)

  alt <- match.arg(alternative)

  if (alt == "all") {
    sig <- c("both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <- list(
    n1 = n1,
    n2 = n2,
    phat1 = round(phat1, 3),
    phat2 = round(phat2, 3),
    z = round(z, 3),
    sig = round(sig, 3),
    alt = alt
  )

  class(out) <- "infer_ts_prop_test"
  return(out)
}

#' @export
#' @rdname infer_ts_prop_test
#' @usage NULL
#'
ts_prop_calc <- function(n1, n2, p1, p2,
                         alternative = c("both", "less", "greater", "all"), ...) {
  .Deprecated("infer_ts_prop_calc()")
  infer_ts_prop_calc(n1, n2, p1, p2, alternative, ...)
}
