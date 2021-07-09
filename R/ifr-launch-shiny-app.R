#' @title Launch Shiny App
#' @description Launches shiny app
#' @examples
#' \dontrun{
#' ifr_launch_shiny_app()
#' }
#' @export
#'
ifr_launch_shiny_app <- function() {

	message("`ifr_launch_shiny_app()` has been soft-deprecated and will be removed in the next release. In future, to launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_inference()\n")

	check_suggests('descriptr')
	check_suggests('jsonlite')
	check_suggests('haven')
	check_suggests('lubridate')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('xplorerr')
	
	xplorerr::app_inference()

}