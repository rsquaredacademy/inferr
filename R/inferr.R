#' \code{inferr} package
#'
#' Parametric and non parametric statistical tests
#'
#' See the README on
#' \href{https://github.com/rsquaredacademy/inferr}{GitHub}
#'
#' @docType package
#' @name inferr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "sse", "sst", "var", "sd"))
