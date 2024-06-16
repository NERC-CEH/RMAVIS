#' Run the `RMAVIS` R Shiny app
#' 
#' Run the `RMAVIS` R Shiny app following the installation of `RMAVIS` as a 
#' package.
#'
#' @export
#'
#' @examples
#' RMAVIS::runApp()
runApp <- function() {
  
  appDir <- system.file("app", "rmavis_app.R", package = "RMAVIS")
  
  if(appDir == ""){
    
    stop("Could not find the application. Try re-installing `RMAVIS`.", call. = FALSE)
    
  }
  
  shiny::runApp(appDir, display.mode = "normal")
  
}