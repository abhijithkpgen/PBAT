# tests/testthat/test_eda_module.R

test_that("EDA UI constructs (both tabs)", {
  ui1 <- analysisUI("eda_test")[[1]]
  ui2 <- analysisUI("eda_test")[[2]]
  expect_s3_class(ui1, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
  expect_s3_class(ui2, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("Home UI constructs", {
  ui_home <- homeUI("home_test")
  expect_s3_class(ui_home, c("shiny.tag", "shiny.tag.list"), exact = FALSE)
})

test_that("analysisServer initializes with reactive home_inputs", {
  # minimal reactive that mimics homeServer() output
  home_inputs <- shiny::reactiveVal(NULL)

  # initialize once with NULL (no click yet)
  expect_silent({
    shiny::testServer(
      app = analysisServer,
      args = list(id = "eda_test", home_inputs = home_inputs),
      { TRUE }
    )
  })

  # now mimic a click from Home: provide analysis_mode
  home_inputs(list(analysis_mode = "eda"))
  expect_silent({
    shiny::testServer(
      app = analysisServer,
      args = list(id = "eda_test", home_inputs = home_inputs),
      { TRUE }
    )
  })
})

test_that("run_app returns a shiny app object", {
  app <- PBAT::run_app()
  expect_s3_class(app, "shiny.appobj")
})
