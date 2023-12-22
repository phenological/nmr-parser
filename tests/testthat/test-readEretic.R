test_that("reading inexisting file", {
  path <- system.file("..", "tests", "testthat", "QuantFactorSample", package = "nmr.parser")
  eretic <- readEretic(path)
  expect_equal(eretic, NULL)
})

test_that("reading eretic file", {
  path <- system.file("..", "tests", "testthat", "QuantFactorSample.xml", package = "nmr.parser")
  eretic <- readEretic(path)
  expect_equal(eretic$field, 600)
})
