test_that("reading inexistent file", {
  path <- system.file("..", "tests", "testthat", "lipo_results", package = "nmr.parser")
  lipo <- roldx_getLipo(path)
  expect_equal(lipo, NULL)
})

test_that("reading file", {
  path <- system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser")
  lipo <- roldx_getLipo(path)
  expect_equal(lipo$id[1], "TPTG")
  expect_equal(lipo$value[1], 69.49)
  expect_equal(length(lipo$value), 112)
})



