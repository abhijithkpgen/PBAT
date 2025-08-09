#' Run the PBAT Shiny App
#'
#' @return Invisibly returns the app object.
#' @examplesIf interactive()
#' PBAT::run_pbat()
#' @export
run_pbat <- function(...) {
  shiny::runApp(pbat_app(), ...)
}
