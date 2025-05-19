test_that("reading inexistent file", {
  param <- readParams(system.file("HB-COVID0001", "10", "acq", package = "nmr.parser"))
  expect_equal(param, NULL)
})

test_that("checking dimensions", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 1128)
  expect_equal(ncol(param), 3)
})

test_that("checking PULPROG", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "PULPROG"], "noesygppr1d")
})

test_that("checking USERA2", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "USERA2"], "")
})

test_that("checking D1", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "D_1"], "4")
})

test_that("checking NPOINTS", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "NPOINTS"], "13 modification sequence number")
})

test_that("checking instrument", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrument"], "nmrsu-rmn601")
})

test_that("checking date", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentDate"], "2020-07-01")
})

test_that("checking time", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTime"], "16:55:49.592")
})

test_that("checking timezone", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTimeZone"], "+0200")
})

test_that("checking timezone xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTimeZone"], "BST (UT+1h)")
})

test_that("checking time xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTime"], "12:08:08")
})

test_that("checking date xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentDate"], "Wed Jun 2 2004")
})

test_that("checking instrument xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrument"], "comet-bc-jkn-17-sk-med-ic-ac-uk")
})

test_that("handling amix files", {
  param <- readParams(system.file("hydroxyproline", "1dno-d2o-7-g", "acqus", package = "nmr.parser"))
  expect_equal(param, NULL)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20-10-acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 901)
  expect_equal(ncol(param), 3)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20", "10", "acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 901)
  expect_equal(ncol(param), 3)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20", "10", "pdata" , "1", "procs", package = "nmr.parser"))
  expect_equal(param, NULL)
})
