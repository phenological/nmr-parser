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
  expect_equal(param$value[param$name == "instrumentDate"], "2020-07-01")
})

test_that("checking time", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrumentTime"], "16:55:49.592")
})

test_that("checking timezone", {
  path <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrumentTimeZone"], "+0200")
})

test_that("checking timezone xwinnmr", {
  path <- system.file("xwinnmr_acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrumentTimeZone"], "BST (UT+1h)")
})

test_that("checking time xwinnmr", {
  path <- system.file("xwinnmr_acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrumentTime"], "12:08:08")
})

test_that("checking date xwinnmr", {
  path <- system.file("xwinnmr_acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrumentDate"], "Wed Jun 2 2004")
})

test_that("checking instrument xwinnmr", {
  path <- system.file("xwinnmr_acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param$value[param$name == "instrument"], "comet@bc-jkn-17.sk.med.ic.ac.uk")
})

test_that("handling amix files", {
  path <- system.file("hydroxyproline", "1dno-d2o-7-g", "acqus", package = "nmr.parser")
  param <- readParams(path)
  expect_equal(param, NULL)
})
