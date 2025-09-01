#' Launch the PBAT Shiny app
#'
#' @export
#' @importFrom shiny shinyApp addResourcePath
run_app <- function() {
  # Add the path to the 'www' directory inside the installed package
  shiny::addResourcePath("www", system.file("app/www", package = "PbAT"))
  
  # Launch the app
  shiny::shinyApp(ui = app_ui(), server = app_server)
}