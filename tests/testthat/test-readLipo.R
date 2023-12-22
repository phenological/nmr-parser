test_that("reading inexistent file", {
  path <- system.file("..", "tests", "testthat", "lipo_results", package = "nmr.parser")
  lipo <- readLipo(path)
  expect_equal(lipo, NULL)
})

test_that("reading file", {
  path <- system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser")
  lipo <- readLipo(path)
  expect_equal(lipo$data$id[1], "TPTG")
  expect_equal(lipo$data$fraction[1], "Main Parameters")
  expect_equal(lipo$data$value[1], 69.49)
  expect_equal(length(lipo$data$value), 112)
  expect_equal(lipo$version, "PL-5009-01/001")
})
