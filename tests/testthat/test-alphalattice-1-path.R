test_that("alphalattice example file is installed", {
  path <- system.file("extdata", "Alpha_lattice_sample.csv", package = "PBAT")
  expect_true(file.exists(path))
})
