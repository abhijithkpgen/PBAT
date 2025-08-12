#' Launch the PBAT Shiny app
#'
#' Sets up static resources bundled in the package and starts the app.
#'
#' @param ... Passed to [shiny::shinyApp()]
#' @return A Shiny app object.
#' @export
#' @importFrom shiny shinyApp addResourcePath
run_app <- function(...) {

  # ---- 1) Static assets ----
  # Installed package path: inst/app/www -> system.file("app/www", ...)
  pkg_www <- system.file("app/www", package = "PBAT")

  # Dev (source) path: repo/inst/app/www
  dev_www1 <- file.path(getwd(), "inst", "app", "www")
  dev_www2 <- file.path(getwd(), "www")  # optional second fallback

  www_path <- if (nzchar(pkg_www) && dir.exists(pkg_www)) {
    pkg_www
  } else if (dir.exists(dev_www1)) {
    dev_www1
  } else if (dir.exists(dev_www2)) {
    dev_www2
  } else {
    NULL
  }

  if (!is.null(www_path)) {
    # Map URL prefix "www" -> actual disk path
    shiny::addResourcePath("www", www_path)
  }

  # ---- 2) Dev quality-of-life ----
  if (interactive()) {
    options(shiny.autoreload = TRUE)
  }

  # ---- 3) Host/port (useful for Docker too) ----
  port <- as.numeric(Sys.getenv("PORT", "8100"))
  opts <- list(host = "0.0.0.0", port = port)

  # ---- 4) Launch ----
  shiny::shinyApp(
    ui      = app_ui(),
    server  = app_server,
    options = opts,
    ...
  )
}
