# tests/testthat/test_multivariate_module.R

test_that("multivariate UI constructs without error", {
  ui <- multivariate_analysis_ui(id = "multi_1")
  # Should be a shiny tag/list
  expect_s3_class(ui, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("home UI constructs without error", {
  ui_home <- homeUI(id = "home_1")
  expect_s3_class(ui_home, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("module server can be initialized with minimal shared data", {
  # Minimal shared_data object the server expects
  shared_data <- shiny::reactiveValues(file_data = NULL, multi_subtype = NULL)

  # Initialize the module server; this should not error even with NULL data
  expect_silent({
    shiny::testServer(
      app = multivariate_analysis_server,
      args = list(id = "multi_1", shared_data = shared_data),
      {
        # No assertions inside yet; just exercising initialization path.
        # You can add expectations on outputs/reactives if the module exposes them.
        TRUE
      }
    )
  })
})

test_that("run_app returns a shiny app object", {
  app <- PBAT::run_app()  # constructs the app object; doesn't launch
  expect_s3_class(app, "shiny.appobj")
})
