
test_that("reading plasma_quant_report_2_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report_2_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Ethanol")
  expect_equal(quant$data$rawConc[1], "0.624")  
  expect_equal(nrow(quant$data), 41)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "2.1.0+release.QuantPS_2.1.b9ea40f Research Use Only")
})

test_that("reading plasma_quant_report.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Ethanol")
  expect_equal(quant$data$rawConc[1], "0.043")
  expect_equal(quant$data$sigCorr[1], "50")
  expect_equal(nrow(quant$data), 41)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-PS 2.0.0  Reseach Use Only")
})

test_that("reading urine_quant_report_b_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_b_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 50)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR B.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_b.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_b.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 50)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR B.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_e_1_2_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e_1_2_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "3.307")
  expect_equal(quant$data$sigCorr[1], '100')
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "1.2.0+release.QuantUR_E_1.2.f3ad700 Research Use Only")
})

test_that("reading urine_quant_report_e_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR E.1.1.0  Reseach Use Only")
})


test_that("reading urine_quant_report_e.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR E.1.1.0  Reseach Use Only")
})


test_that("reading urine_quant_report_ne_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_ne_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR NE.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_ne.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_ne.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR NE.1.1.0  Reseach Use Only")
})

