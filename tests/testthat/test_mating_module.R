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
  withr::local_options(shiny.testmode = TRUE)  # be explicit for testServer
  shared <- shiny::reactiveValues(file_data = NULL, mating_design = NULL)

  expect_no_error(
    suppressWarnings(suppressMessages(
      shiny::testServer(
        app = mating_design_server,
        args = list(id = "mating_design_1", shared_data = shared),
        {
          # bootstrap only — just ensure it wires up without throwing
          TRUE
        }
      )
    ))
  )
})

test_that("run_app returns a shiny app object", {
  withr::local_options(shiny.testmode = TRUE)
  app <- run_app()                 # no need to namespace-call inside your own pkg
  expect_s3_class(app, "shiny.appobj")
})
