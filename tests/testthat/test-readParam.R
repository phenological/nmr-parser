
test_that("reading PULPROG", {
  param <- readParam(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"), "PULPROG")
  expect_equal(param, "noesygppr1d")
})

test_that("reading inexistent file", {
  param <- readParam(system.file("HB-COVID0001", "10", "acq", package = "nmr.parser"), "PULPROG")
  expect_equal(param, NULL)
})

test_that("reading param is empty returns null", {
  param <- readParam(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"), "USERA2")
  expect_equal(param, "")
})

