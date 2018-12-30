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
   rlang::abort("The shiny app has been moved to a new package, `xplorerr`. To launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_inference()")
}
 
#' @export
#' @rdname infer_launch_shiny_app
#' @usage NULL
#'
launch_inferr <- function() {
  .Deprecated("infer_launch_shiny_app()")
  infer_launch_shiny_app()
}