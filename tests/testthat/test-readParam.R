
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

test_that("reading several params at once", {
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("EXP", "PULPROG", "BF1"))

  expect_length(prms, 3)
  expect_equal(prms[[1]], "PROF_PLASMA_NOESY")
  expect_equal(prms[[2]], "noesygppr1d")
  expect_equal(prms[[3]], 600.27)
})

test_that("a param missing among several comes back as NA in place", {
  #the guard on this branch read length(idx > 0), which measures the logical
  #vector rather than the match, and only reached the else by way of
  #length(logical(0)) being 0
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("PULPROG", "NOSUCHPARAM", "BF1"))

  expect_length(prms, 3)
  expect_equal(prms[[1]], "noesygppr1d")
  expect_true(is.na(prms[[2]]))
  expect_equal(prms[[3]], 600.27)

  #the surviving params keep their position, so callers indexing by rank
  #(scanFolder does) are not shifted by the missing one
  expect_equal(names(prms), NULL)
  expect_length(readParam(acqus, c("PULPROG", "NOSUCHPARAM")), 2)
})

test_that("a missing first param does not discard the rest", {
  #the whole file used to be tested on paramName[1] only, so an acqus
  #without an EXP line returned NULL and scanFolder() died indexing prms[[1]]
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("NOSUCHPARAM", "PULPROG"))

  expect_length(prms, 2)
  expect_true(is.na(prms[[1]]))
  expect_equal(prms[[2]], "noesygppr1d")
})

test_that("a param absent from every position still returns NA in place", {
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("NOSUCHPARAM", "NOSUCHEITHER"))

  expect_length(prms, 2)
  expect_true(all(is.na(unlist(prms))))
})
