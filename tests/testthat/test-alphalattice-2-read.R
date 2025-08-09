test_that("alphalattice CSV can be read", {
  path <- system.file("extdata", "Alpha_lattice_sample.csv", package = "PBAT")
  skip_if_not(file.exists(path), "example CSV missing")

  dat <- readr::read_csv(path, show_col_types = FALSE)

  expect_s3_class(dat, "data.frame")
  expect_gt(nrow(dat), 10)         # should have some rows
  expect_gt(ncol(dat), 3)          # and multiple columns
})
