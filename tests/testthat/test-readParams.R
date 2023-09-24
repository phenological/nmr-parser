test_that("reading inexistent file", {
  path <- system.file("HB-COVID0001", "10", "acq", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param, NULL)
})

test_that("checking dimensions", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(nrow(param), 1127)
  expect_equal(ncol(param), 3)
})

test_that("checking PULPROG", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "PULPROG"], "noesygppr1d")
})

test_that("checking USERA2", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "USERA2"], "")
})

test_that("checking D1", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "D_1"], "4")
})

test_that("checking NPOINTS", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "NPOINTS"], "13 modification sequence number")
})

test_that("checking instrument", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrument"], "nmrsu@rmn601")
})

test_that("checking date", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "date"], "2020-07-01")
})

test_that("checking time", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "time"], "16:55:49.592")
})

test_that("checking timezone", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "timezone"], "+0200")
})

