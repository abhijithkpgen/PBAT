test_that("run_pbat exists and is callable", {
  testthat::skip_on_ci()   # <— prevents Shiny launch on GitHub
  expect_true(is.function(PBAT::run_pbat))
})