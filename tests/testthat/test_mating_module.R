# tests/testthat/test_mating_module.R

test_that("mating design UI constructs", {
  ui <- mating_design_ui(id = "mating_design_1")
  expect_s3_class(ui, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("home UI constructs", {
  ui_home <- homeUI(id = "home_1")
  expect_s3_class(ui_home, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("mating module server initializes with minimal shared data", {
  shared <- shiny::reactiveValues(file_data = NULL, mating_design = NULL)

  expect_silent({
    shiny::testServer(
      app = mating_design_server,
      args = list(id = "mating_design_1", shared_data = shared),
      {
        # No assertions yet—this just ensures server initializes without error.
        TRUE
      }
    )
  })
})

test_that("run_app returns a shiny app object", {
  app <- PBAT::run_app()
  expect_s3_class(app, "shiny.appobj")
})
