test_that("reading inexistent file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report", package = "nmr.parser"))
  expect_equal(quant, NULL)
})

test_that("reading file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Ethanol")
  expect_equal(quant$data$rawConc[1], "0.000")
  expect_equal(quant$data$sigCorr[1], "0")
  expect_equal(nrow(quant$data), 41)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-PS 2.0.0  Reseach Use Only")
})
