#' Run app
#' 
#' Run the Nepal Shiny web application
#' @return Web application served
#' @importFrom shiny runApp
#' @export

run_app <- function(){
  app_location <- paste0(system.file(package = 'nepal'), '/shiny/app.R')
  shiny::runApp(app_location)
}
