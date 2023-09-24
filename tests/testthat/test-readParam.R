
test_that("reading PULPROG", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParam(path, "PULPROG")
  expect_equal(param, "noesygppr1d")
})

test_that("reading inexistent file", {
  path <- system.file("HB-COVID0001", "10", "acq", package = "nmr.parser")
  param <- readParam(path, "PULPROG")
  expect_equal(param, NULL)
})

test_that("reading param is empty returns null", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParam(path, "USERA2")
  expect_equal(param, "")
})

