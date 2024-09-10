test_that("reading inexistent file", {
  qc <- readQc(system.file("..", "tests", "testthat", "plasma_qc_report", package = "nmr.parser"))
  expect_equal(qc, NULL)
})

test_that("reading file", {
  qc <- readQc(system.file("..", "tests", "testthat", "plasma_qc_report.xml", package = "nmr.parser"))
  qc <- qc$data
  expect_equal(length(qc), 4)
  expect_equal(length(qc$infos), 2)
  expect_equal(length(qc$infoNames), 24)
  expect_equal(length(qc$tests), 7)
  expect_equal(length(qc$testNames), 22)
  expect_equal(qc$testNames[1], "linewidth-in-hz")
  expect_equal(qc$infoNames[1], "nmr-experiment-quality-test")
  expect_equal(qc$infos$name[1], "NMR Experiment Quality Test")
  expect_equal(qc$infos$value[1], "not passed" )
  expect_equal(qc$tests$comment[1], "not passed")
  expect_equal(qc$tests$value[1], "1.6")
  expect_equal(length(qc$tests[[1]]), 22)
  expect_equal(qc$testNames[2], "residual-water-signal-in-mmol-l")
  expect_equal(qc$tests$name[2], "Residual Water Signal in mmol/L")
  expect_equal(qc$tests$type[22], "Contamination Test")
})
