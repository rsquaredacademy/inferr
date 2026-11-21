fg <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

fk <- function(x, w) {
  format(x, width = w, justify = "centre", nsmall = 3)
}


fs <- function() {
  rep("  ")
}

fl <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "left")
}

fc <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formatter_t <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_cil <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_ciu <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formats_t <- function() {
  rep("  ")
}

formatter_pair <- function(x, w) {
  x1  <- format(x, nsmall = 2)
  x2  <- as.character(x1)
  ret <- format(x2, width = w, justify = "centre")
  return(ret)
}

fw <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

fn <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formats <- function() {
  rep("    ")
}

#' @importFrom utils packageVersion menu install.packages
check_suggests <- function(pkg) {
  
  pkg_flag <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  
  if (is.na(pkg_flag)) {
    
    msg <- paste0('\n', pkg, ' must be installed for this functionality.')
    
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    } 
  }

}

check_binary <- function(x, name) {

  if (is.factor(x)) {
    ok <- nlevels(x) == 2
  } else {
    ok <- length(unique(x[!is.na(x)])) == 2
  }

  if (!ok) {
    stop(paste0(name, " must be a binary variable with exactly two levels."),
         call. = FALSE)
  }

  invisible(x)

}

# confidence levels and type I error rates share the same open interval; a
# single helper keeps every user-facing message and range consistent
check_probability <- function(x, name) {

  if (!is.numeric(x) || length(x) != 1L) {
    stop(paste0(name, " must be numeric"), call. = FALSE)
  }

  if (!is.finite(x) || x <= 0 || x >= 1) {
    stop(paste0(name, " must be between 0 and 1 (exclusive)"),
         call. = FALSE)
  }

  invisible(x)

}

# Satterthwaite degrees of freedom are printed with two decimals, while the
# stored value remains unrounded
format_satt_df <- function(x) {
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

# Supported numeric domain shared by the inferential interfaces: finite data
# with |x| <= 1e150 and sample sizes <= 1e15. The bounds are deliberately wide
# enough for teaching data and narrow enough that the moments used by the tests
# stay inside double precision.
check_finite_range <- function(x, name) {

  if (!is.numeric(x)) {
    return(invisible(x))
  }

  if (anyNA(x) || any(!is.finite(x)) || any(abs(x) > 1e150)) {
    stop(paste0(name, " must contain only finite values with absolute value ",
                "<= 1e150"), call. = FALSE)
  }

  invisible(x)
}

check_max_sample_size <- function(n, name) {

  if (length(n) != 1L || !is.finite(n) || n > 1e15) {
    stop(paste0(name, " must be a finite sample size <= 1e15"),
         call. = FALSE)
  }

  invisible(n)
}

check_finite_moment <- function(value, moment, name) {

  if (length(value) == 0L) {
    return(invisible(value))
  }

  if (any(!is.finite(value))) {
    stop(paste0("computed ", moment, " for '", name,
                "' is not finite; '", name,
                "' must contain finite values with absolute value <= 1e150 ",
                "and the sample size must be <= 1e15"),
         call. = FALSE)
  }

  invisible(value)
}

# Print a p-value at the printer's display resolution. A positive p-value
# smaller than the last displayed decimal prints as a bound instead of a false
# exact zero. `raw` is the unrounded p-value when the object stores a rounded
# one.
format_p_bound <- function(p, raw = NULL, digits = 4L, nsmall = NULL,
                           scientific = FALSE, statistic = NULL) {

  if (is.null(raw)) {
    raw <- p
  }

  # a zero tail with a finite non-zero statistic is an underflow, not an exact
  # zero; treat it as below the display resolution so the printer shows a bound
  if (!is.null(statistic)) {
    under <- !is.na(raw) & raw == 0 & is.finite(statistic) & statistic != 0
    if (any(under)) {
      raw[under] <- .Machine$double.xmin
    }
  }

  resolution <- 10 ^ (-digits)

  if (is.null(nsmall)) {
    out <- as.character(p)
  } else {
    out <- format(round(p, digits), nsmall = nsmall,
                  scientific = scientific, trim = TRUE)
  }

  # an undefined p-value or label must still have a printable width; several
  # printers take nchar() of this result before formatting
  if (length(p) > 0L) {
    out[is.na(p)] <- "NA"
  }

  tiny <- !is.na(raw) & is.finite(raw) & raw > 0 & raw < resolution
  if (any(tiny)) {
    out[tiny] <- paste0("< ",
                        format(resolution, scientific = FALSE, trim = TRUE))
  }

  out
}

# Pooled two-sample z statistic. The direct pooled-variance product underflows
# when the pooled proportion is subnormal or when it rounds to one, so the
# original expression is kept for ordinary inputs and a factored square root is
# used only when the direct denominator is zero or non-finite.
prop_z_2sample <- function(n1, n2, p1, p2) {

  n_total <- n1 + n2
  p_sum   <- n1 * p1 + n2 * p2
  q_sum   <- n1 * (1 - p1) + n2 * (1 - p2)
  phat    <- p_sum / n_total
  den2    <- (1 / n1) + (1 / n2)
  den     <- sqrt(phat * (1 - phat) * den2)

  if (!is.finite(den) || den == 0) {
    # p_sum / n_total can underflow below the smallest subnormal even when
    # p_sum itself is representable; keep the numerator and complement separate
    den <- sqrt(p_sum) * sqrt(q_sum) / n_total * sqrt(den2)
  }

  (p1 - p2) / den
}

# One-sample z statistic with the same fallback for a subnormal null
# probability whose product with its complement underflows.
prop_z_1sample <- function(n, p, phat) {

  den <- sqrt((p * (1 - p)) / n)

  if (!is.finite(den) || den == 0) {
    den <- sqrt(p) * sqrt(1 - p) / sqrt(n)
  }

  (phat - p) / den
}
