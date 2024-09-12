test_that("reading inexistent file", {
  qc <- readQc(system.file("..", "tests", "testthat", "plasma_qc_report", package = "nmr.parser"))
  expect_equal(qc, NULL)
})

test_that("reading file", {
  qc <- readQc(system.file("..", "tests", "testthat", "plasma_qc_report.xml", package = "nmr.parser"))
  qc <- qc$data
  expect_equal(length(qc), 4)
  expect_equal(length(qc$infos), 3)
  expect_equal(length(qc$infoNames), 24)
  expect_equal(length(qc$tests), 7)
  expect_equal(length(qc$testNames), 22)
  expect_equal(qc$testNames[1], "linewidth-in-hz")
  expect_equal(qc$infoNames[1], "nmr-experiment-quality-test")
  expect_equal(qc$infos$name[1], "NMR Experiment Quality Test")
  expect_equal(is.na(qc$infos$value[1]), TRUE )
  expect_equal(qc$tests$comment[1], "not passed")
  expect_equal(qc$tests$value[1], "1.6")
  expect_equal(length(qc$tests[[1]]), 22)
  expect_equal(qc$testNames[2], "residual-water-signal-in-mmol-l")
  expect_equal(qc$tests$name[2], "Residual Water Signal in mmol/L")
  expect_equal(qc$tests$type[22], "Contamination Test")
})


test_that("reading file", {
  qc <- readQc(system.file("..", "tests", "testthat", "urine_qc_report.xml", package = "nmr.parser"))
  qc <- qc$data
  expect_equal(length(qc), 4)
  expect_equal(length(qc$infos), 3)
  expect_equal(length(qc$infoNames), 27)
  expect_equal(length(qc$tests), 7)
  expect_equal(length(qc$testNames), 28)
  expect_equal(qc$testNames[1], "linewidth-in-hz")
  expect_equal(qc$infoNames[1], "nmr-experiment-quality-test")
  expect_equal(qc$infos$name[1], "NMR Experiment Quality Test")
  expect_equal(is.na(qc$infos$value[1]), TRUE )
  expect_equal(qc$tests$comment[1], "passed")
  expect_equal(qc$tests$value[1], "1.0")
  expect_equal(length(qc$tests[[1]]), 28)
  expect_equal(qc$testNames[2], "line-asymmetry-in-hz")
  expect_equal(qc$tests$name[2], "Line Asymmetry in Hz")
  expect_equal(qc$tests$type[22], "Medication Test")
})

