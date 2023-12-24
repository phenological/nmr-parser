test_that("reading inexistent file", {
  path <- system.file("..", "tests", "testthat", "plasma_pacs_report", package = "nmr.parser")
  pacs <- readPacs(path)
  expect_equal(pacs, NULL)
})

test_that("reading file", {
  path <- system.file("..", "tests", "testthat", "plasma_pacs_report.xml", package = "nmr.parser")
  pacs <- readPacs(path)
  expect_equal(pacs$data$name[1], "Glucose")
  expect_equal(pacs$data$conc_v[1], "4.808")
  expect_equal(pacs$data$refMax[1], "6.08")
  expect_equal(pacs$data$refMin[1], "1.73")
  expect_equal(nrow(pacs$data), 16)
  expect_equal(ncol(pacs$data), 6)
  expect_equal(pacs$version, "PhenoRisk PACS RuO 1.0.0  Research Use Only")
})
