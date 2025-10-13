test_that("reading inexistent file", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results", package = "nmr.parser"))
  expect_equal(lipo, NULL)
})

test_that("reading file", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  # names(lipo$data)
  expect_equal(lipo$data$id[1], "TPTG")
  expect_equal(lipo$data$fraction[1], "Main Parameters")
  expect_equal(lipo$data$value[1], 99.680)
  expect_equal(length(lipo$data$value), 112)
  expect_equal(ncol(lipo$data), 10)
  expect_equal(lipo$version, "PL-5009-01/001")
})


test_that("reading file", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  lipo <- extend_lipo(lipo)
  expect_equal(lipo$data$id[1], "TPTG")
  expect_equal(lipo$data$fraction[1], "Main Parameters")
  expect_equal(lipo$data$value[1], 99.680)
  expect_equal(length(lipo$data$value), 316)
  expect_equal(ncol(lipo$data), 11)
  expect_equal(lipo$version, "PL-5009-01/001")
})

test_that("reading file", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "plasma_lipo_report_1_1_0.xml", package = "nmr.parser"))
  lipo <- extend_lipo(lipo)
  expect_equal(lipo$data$id[1], "TPTG")
  expect_equal(lipo$data$fraction[1], "Main Parameters")
  expect_equal(lipo$data$value[1], 58.260)
  expect_equal(length(lipo$data$value), 316)
  expect_equal(ncol(lipo$data), 11)
  expect_equal(lipo$version, "PL-5009-01/002")
})


