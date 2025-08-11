#' Launch the PBAT Shiny app
#'
#' Sets up static resources bundled in the package and starts the app.
#'
#' @param ... Passed to [shiny::shinyApp()]
#' @return A Shiny app object.
#' @export
run_app <- function(...) {

  # Serve package assets so paths like "LogoNobg.png" and CSV links work.
  # Assets are stored under inst/app/www in the package.
  www_dir <- system.file("app/www", package = "PBAT")
  if (nzchar(www_dir) && dir.exists(www_dir)) {
    # Map "www" so URLs like "www/LogoNobg.png" resolve
    shiny::addResourcePath("www", www_dir)
  }

  # Get the port from the environment variable provided by Cloud Run.
  # Default to 8100 if the PORT variable is not set (for local development).
  port <- as.numeric(Sys.getenv("PORT", "8100"))

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    # Set host to '0.0.0.0' to be accessible within the container
    # and use the port determined above.
    options = list(host = '0.0.0.0', port = port)
  )
}
