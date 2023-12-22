test_that("reading inexistent file", {
  path <- system.file("..", "tests", "testthat", "title", package = "nmr.parser")
  title <- readTitle(path)
  expect_equal(title, NULL)
})

test_that("reading title single line", {
  path <- system.file("..", "tests", "testthat", "title_singleline", package = "nmr.parser")
  title <- readTitle(path)
  expect_equal(title$path, "title")
  expect_equal(title$name, "title")
  expect_equal(title$value, "TITLE 32565_600")
})

test_that("reading title multiple lines", {
  path <- system.file("..", "tests", "testthat", "title_multiline", package = "nmr.parser")
  title <- readTitle(path)
  expect_equal(title$path, "title")
  expect_equal(title$name, "title")
  expect_equal(title$value, "NOESYGPPR1D Bodyfluid NMR SOP validation\n---------------------------------------------------\nD:/IVDrData/data/CL-PS-SOLOMON_PARK_RESEARCH_LABS-230227-230918/nmr/32658_600/10/pdata/1/1r\nAcquisition at: 2023-09-18-15-13-16\nProbe head: Z814601_0079 (PA BBI 600S3 H-BB-D-05 Z)\nSample info:    5mm Plasma sample\nParameter set: PROF_PLASMA_NOESY\nSolvent: Plasma\n---------------------------------------------------\nPulse P1:  10.07us\nPower PLdB1: -11.14dB\nPower PLdB9:  48.80dB\nRF Presat:  25.00Hz\nFrequency Offset O1: 2823.15Hz\nHalfwidth of Alanine (<=1.50Hz):   0.99Hz\nResidual Solvent Signal (<=30.00mmol/L):  16.67mmol/L\nBodyfluid NMR parameters within acceptance range")
})
