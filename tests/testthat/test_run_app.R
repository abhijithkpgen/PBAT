test_that("run_app returns a shiny app", {
  app <- PBAT::run_app()
  expect_s3_class(app, "shiny.appobj")
})
