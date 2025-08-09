#' Launch the PBAT Shiny App
#'
#' @param ... Additional arguments passed to shiny::runApp()
#' @export
run_pbat <- function(...) {
  shiny::runApp(system.file("app", package = "PBAT"), ...)
}
