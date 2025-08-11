#' Launch the PBAT Shiny app
#'
#' Sets up static resources bundled in the package and starts the app.
#'
#' @param ... Passed to [shiny::shinyApp()] (currently unused).
#' @return A Shiny app object (in interactive sessions it will launch).
#' @export
run_app <- function(...) {

  # Serve package assets so paths like "LogoNobg.png" and CSV links work.
  # Assets are stored under inst/app/www in the package.
  www_dir <- system.file("app/www", package = "PBAT")
  if (nzchar(www_dir) && dir.exists(www_dir)) {
    # Map "www" so URLs like "www/LogoNobg.png" resolve
    shiny::addResourcePath("www", www_dir)
  }

  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
