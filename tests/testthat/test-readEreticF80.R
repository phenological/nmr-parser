test_that("reading inexisting file", {
  eretic <- readEreticF80(system.file("..", "tests", "testthat", "QuantFactorSample", package = "nmr.parser"))
  expect_equal(eretic, NULL)
})

test_that("reading eretic file", {
  eretic <- readEreticF80(system.file("..", "tests", "testthat", "eretic_file.xml", package = "nmr.parser"))
  expect_equal(eretic$field, 80)
})
