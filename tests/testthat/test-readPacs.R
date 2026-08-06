test_that("reading inexistent file", {
  pacs <- readPacs(system.file("..", "tests", "testthat", "plasma_pacs_report", package = "nmr.parser"))
  expect_equal(pacs, NULL)
})

test_that("reading file", {
  pacs <- readPacs(system.file("extdata", "plasma_pacs_report.xml", package = "nmr.parser"))
  expect_equal(pacs$data$name[1], "Glucose")
  expect_equal(pacs$data$conc_v[1], "4.948")
  expect_equal(pacs$data$refMax[1], "6.08")
  expect_equal(pacs$data$refMin[1], "1.73")
  expect_equal(nrow(pacs$data), 16)
  expect_equal(ncol(pacs$data), 6)
  expect_equal(pacs$version, "PhenoRisk PACS RuO 1.0.0  Research Use Only")
})


test_that("reading file", {
  pacs <- readPacs(system.file("..", "tests", "testthat", "plasma_pacs_report_1_1_0.xml", package = "nmr.parser"))
  expect_equal(pacs$data$name[1], "Glucose")
  expect_equal(pacs$data$conc_v[1], "5.035")
  expect_equal(pacs$data$refMax[1], "6.08")
  expect_equal(pacs$data$refMin[1], "1.73")
  expect_equal(nrow(pacs$data), 16)
  expect_equal(ncol(pacs$data), 6)
  expect_equal(pacs$version, "1.1.0+release.PhenoriskPCS_1.1.7fe55b9 Research Use Only")
})

test_that("getPacsTable reads its report from the installed extdata", {
  #it used to reach into ../tests/testthat, which is not installed
  expect_true(file.exists(system.file("extdata", "plasma_pacs_report.xml", package = "nmr.parser")))
  expect_s3_class(getPacsTable(), "data.frame")
})
