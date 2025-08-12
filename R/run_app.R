#' Launch the PBAT Shiny app
#'
#' Sets up static resources bundled in the package and starts the app.
#'
#' @param ... Passed to [shiny::shinyApp()]
#' @return A Shiny app object.
#' @export
#' @importFrom shiny shinyApp addResourcePath
run_app <- function(...) {
  
  # 1) Serve static assets from the built package
  pkg_www <- system.file("app/www", package = "PBAT")
  if (nzchar(pkg_www) && dir.exists(pkg_www)) {
    shiny::addResourcePath("www", pkg_www)
  } else {
    # 2) Dev fallback: use repo's ./www if running from source
    dev_www <- file.path(getwd(), "www")
    if (dir.exists(dev_www)) {
      shiny::addResourcePath("www", dev_www)
    }
  }
  
  # 3) Host/port (works locally and in containers)
  port <- as.numeric(Sys.getenv("PORT", "8100"))
  opts <- list(host = "0.0.0.0", port = port)
  
  # 4) Build and return the app
  shiny::shinyApp(
    ui     = app_ui(),
    server = app_server,
    options = opts,
    ...
  )
}
