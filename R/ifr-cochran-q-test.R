#' @title Cochran Q Test
#' @description Test if the proportions of 3 or more dichotomous variables are
#' equal in the same population. Rows with a missing value in any of the
#' specified variables are removed before the test is computed.
#' @param data a \code{data.frame} or \code{tibble}
#' @param ... columns in \code{data}
#'
#' @return \code{ifr_cochran_qtest} returns an object of class
#' \code{"ifr_cochran_qtest"}. An object of class \code{"ifr_cochran_qtest"}
#' is a list containing the following components:
#'
#' \item{df}{degrees of freedom}
#' \item{n}{number of observations}
#' \item{pvalue}{p value}
#' \item{q}{Cochran's Q statistic}
#'
#' @section Deprecated Function:
#' \code{infer_cochran_test()} has been deprecated. Instead use
#' \code{ifr_cochran_qtest()}.
#'
#' @references Sheskin, D. J. 2007. Handbook of Parametric and Nonparametric
#' Statistical Procedures, 4th edition. : Chapman & Hall/CRC.
#'
#' @examples
#' ifr_cochran_qtest(exam, exam1, exam2, exam3)
#'
#' @export
#'
ifr_cochran_qtest <- function(data, ...) UseMethod("ifr_cochran_qtest")

#' @export
ifr_cochran_qtest.default <- function(data, ...) {

  vars  <- vapply(substitute(...()), deparse, NA_character_)
  fdata <- data[vars]

  if (ncol(fdata) < 3) {
    stop("Please specify at least 3 variables.", call. = FALSE)
  }

  # consistent missing-data policy: a row is used only when every specified
  # variable is observed
  fdata <- fdata[stats::complete.cases(fdata), , drop = FALSE]

  if (nrow(fdata) < 2) {
    stop("at least two complete observations are required", call. = FALSE)
  }

  fdata <- cochran_recode(fdata)

  k <- cochran_comp(fdata)

  result <-
    list(
      df     = k$df,
      n      = k$n,
      pvalue = k$pvalue,
      q      = k$q)

  class(result) <- "ifr_cochran_qtest"
  return(result)
}

#' @export
#' @rdname ifr_cochran_qtest
#' @usage NULL
#'
infer_cochran_qtest <- function(data, ...) {
  .Deprecated("ifr_cochran_qtest()")
}

#' @export
#'
print.ifr_cochran_qtest <- function(x, ...) {
  print_cochran_test(x)
}

coch_data <- function(x, ...) {

  if (is.data.frame(x)) {
    data <- x %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      `-`(1)
  } else {
    data <- cbind(x, ...) %>%
      apply(2, as.numeric) %>%
      `-`(1) %>%
      as.data.frame()
  }

  return(data)
}

cochran_recode <- function(data) {

  # resolve the numeric outcome coding once across all columns, before any
  # homogeneous column is recoded: a column of all 1s can be the success code
  # of a 0/1 design or the failure code of a 1/2 design, and the other columns
  # settle that question (H13)
  num <- vapply(data, is.numeric, logical(1))
  if (any(num)) {
    is01  <- vapply(data[num], function(x) all(x %in% c(0, 1)), logical(1))
    is12  <- vapply(data[num], function(x) all(x %in% c(1, 2)), logical(1))
    valid <- is01 | is12
    if (any(!valid)) {
      stop("Please specify dichotomous/binary variables only.", call. = FALSE)
    }
    use01 <- all(is01)
    use12 <- all(is12)
  } else {
    use01 <- TRUE
    use12 <- FALSE
  }

  if (use01 && !use12) {
    coding <- "01"
  } else if (use12 && !use01) {
    coding <- "12"
  } else if (use01 && use12) {
    # every numeric column is a homogeneous 1: the coding is ambiguous but the
    # statistic is undefined for such a design either way, so keep the 0/1
    # convention
    coding <- "01"
  } else {
    stop(
      paste0(
        "Cannot determine a single 0/1 or 1/2 coding across the numeric ",
        "columns. Use one coding for every column, or supply factors with ",
        "declared levels."
      ),
      call. = FALSE
    )
  }

  out <- lapply(data, function(x) {
    if (is.logical(x)) {
      return(as.integer(x))
    }
    if (is.factor(x)) {
      # use the declared levels: a valid binary factor can still have an
      # unobserved level, either by declaration or after complete-case
      # filtering, and dropping it would lose the success mapping
      if (nlevels(x) != 2) {
        stop("Please specify dichotomous/binary variables only.", call. = FALSE)
      }
      return(as.integer(x) - 1L)
    }
    if (is.numeric(x)) {
      if (coding == "12") {
        return(as.integer(x) - 1L)
      }
      return(as.integer(x))
    }
    stop("Please specify dichotomous/binary variables only.", call. = FALSE)
  })

  as.data.frame(out)
}

cochran_comp <- function(data) {

  n  <- nrow(data)
  k  <- ncol(data)
  df <- k - 1

  cs <-
    data %>%
    lapply(as.numeric) %>%
    as.data.frame() %>%
    sums()

  q <- coch(k, cs$cls_sum, cs$cl, cs$g, cs$gs_sum)

  pvalue <- pchisq(q, df, lower.tail = FALSE)

  list(df     = df,
       n      = n,
       pvalue = round(pvalue, 4),
       q      = q)

}

sums <- function(data) {

  cl      <- colSums(data)
  cls_sum <- sum(cl ^ 2)
  g       <- rowSums(data)
  gs_sum  <- sum(g ^ 2)

  list(cl      = cl,
       cls_sum = cls_sum,
       g       = g,
       gs_sum  = gs_sum)

}

coch <- function(k, cls_sum, cl, g, gs_sum) {
  denom <- (k * sum(g)) - gs_sum

  if (!is.finite(denom) || denom <= 0) {
    stop("Cochran's Q is undefined: the data contain no within-subject discordance",
         call. = FALSE)
  }

  ((k - 1) * ((k * cls_sum) - (sum(cl) ^ 2))) / denom
}