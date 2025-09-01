#' Launch the PBAT Shiny app
#'
#' Sets up static resources bundled in the package and starts the app.
#'
#' @param ... Passed to [shiny::shinyApp()]
#' @return A Shiny app object.
#' @export
#' @importFrom shiny shinyApp addResourcePath
#' @importFrom future plan multisession

run_app <- function(...) {
  
  # ---- 1) Set up the parallel processing plan ----
  future::plan(future::multisession)
  
  # ---- 2) Dev quality-of-life ----
  if (interactive()) {
    options(shiny.autoreload = TRUE)
  }
  
  # ---- 3) Host/port (CORRECTED FOR DEPLOYMENT) ----
  port <- as.numeric(Sys.getenv("PORT", "8100"))
  opts <- list(host = "0.0.0.0", port = port)
  
  # ---- 4) Launch with robust resource path handling ----
  shiny::shinyApp(
    ui      = app_ui(),
    server  = app_server,
    onStart = function() {
      # This function runs once when the app starts.
      # It finds the 'www' directory inside your package's installed
      # location and makes it available at the URL prefix "www".
      # This works both locally (with devtools) and when deployed.
      shiny::addResourcePath(
        "www",
        system.file("app/www", package = "PBAT")
      )
    },
    options = opts,
    ...
  )
}