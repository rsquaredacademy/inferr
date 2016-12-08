#' @importFrom dplyr group_by_ select_ summarise_each
#' @importFrom magrittr %>%
anova_split <- function(data, x, y) {
    by_factor <- data %>%
        group_by_(y) %>%
        select_(y, x) %>%
        summarise_each(funs(length, mean, var, sd)) %>%
        as.data.frame()
    return(by_factor)
}

anova_avg <- function(data, y) {
    avg <- data %>%
        select_(y) %>%
        summarise_each(funs(mean)) %>%
        as.data.frame()
    return(avg)
}

fg <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

fs <- function() {
  rep("  ")
}

fl <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'left')
}

fc <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

formatter_t <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

format_cil <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')}

format_ciu <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = 'centre')
}

formats_t <- function() {
    rep("  ")
}
