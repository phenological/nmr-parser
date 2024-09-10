

getQcTable <- function() {

  qc <- readQc(system.file("..",
                           "tests",
                           "testthat",
                           "plasma_qc_report.xml",
                           package = "nmr.parser"))

  qc <- qc$data

  tbl <- data.frame(testName = qc$tests$name,
                      testType = qc$tests$type,
                      testUnit = qc$tests$unit,
                      testRefMax = qc$tests$refMax,
                      testRefMin = qc$tests$refMin,
                      testDescription = qc$tests$comment)

  tbl2 <- data.frame(testName = qc$infos$name,
                     testType = NA,
                     testUnit = NA,
                     testRefMax = NA,
                     testRefMin = NA,
                     testDescription = qc$infos$value)

  return(rbind(tbl2, tbl1))
}
