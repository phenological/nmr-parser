test_that("reading inexisting file", {
  eretic <- readEretic(system.file("..",
                                   "tests",
                                   "testthat",
                                   "QuantFactorSample",
                                   package = "nmr.parser"))
  expect_equal(eretic, NULL)
})

test_that("reading eretic file", {
  eretic <- readEretic(system.file("..",
                                   "tests",
                                   "testthat",
                                   "QuantFactorSample.xml",
                                   package = "nmr.parser"))
  expect_equal(eretic$field, 600)
})
