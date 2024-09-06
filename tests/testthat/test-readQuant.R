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

test_that("reading file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "8.211")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR E.1.1.0  Reseach Use Only")
})

