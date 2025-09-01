test_that("run_app returns a shiny app", {
  app <- PbAT::run_app()
  expect_s3_class(app, "shiny.appobj")
})
