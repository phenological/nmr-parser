test_that("reading inexisting file", {
  path <- system.file("..", "tests", "testthat", "QuantFactorSample", package = "nmr.parser")
  eretic <- readEreticF80(path)
  expect_equal(eretic, NULL)
})

test_that("reading eretic file", {
  path <- system.file("..", "tests", "testthat", "eretic_file.xml", package = "nmr.parser")
  eretic <- readEreticF80(path)
  expect_equal(eretic$field, 80)
})
