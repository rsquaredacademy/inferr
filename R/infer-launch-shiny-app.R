#' @importFrom shiny runApp
#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' infer_launch_shiny_app()
#' }
#' @export
#'
infer_launch_shiny_app <- function() {
    shiny::runApp(appDir = system.file("application", package = "inferr"))
}

#' @export
#' @rdname infer_launch_shiny_app
#' @usage NULL
#'
launch_inferr <- function() {

    .Deprecated("infer_launch_shiny_app()")
    infer_launch_shiny_app()

}
