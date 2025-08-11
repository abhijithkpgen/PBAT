# tests/testthat/test_multivariate_module.R

test_that("multivariate UI constructs without error", {
  ui <- multivariate_analysis_ui(id = "multi_1")
  expect_s3_class(ui, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("home UI constructs without error", {
  ui_home <- homeUI(id = "home_1")
  expect_s3_class(ui_home, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("module server can be initialized with minimal shared data", {
  # Put Shiny in test mode for deterministic behavior
  old <- options(shiny.testmode = TRUE); on.exit(options(old), add = TRUE)

  shared_data <- shiny::reactiveValues(file_data = NULL, multi_subtype = NULL)

  expect_no_error(
    suppressWarnings(suppressMessages(
      shiny::testServer(
        app = multivariate_analysis_server,
        args = list(id = "multi_1", shared_data = shared_data),
        {
          # Smoke test only — ensure server boots without throwing
          TRUE
        }
      )
    ))
  )
})

test_that("run_app returns a shiny app object", {
  old <- options(shiny.testmode = TRUE); on.exit(options(old), add = TRUE)
  app <- run_app()  # inside the package, no need for PBAT::
  expect_s3_class(app, "shiny.appobj")
})
