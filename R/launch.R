#' @importFrom shiny runApp
#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' launch_inferr()
#' }
#' @export
#'
launch_inferr <- function() {
    shiny::runApp(appDir = system.file("application", package = "inferr"))
}
