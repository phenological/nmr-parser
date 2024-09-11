

getQcTable <- function(matrixType = "SER", withValue = FALSE) {

  if (matrixType == "SER") {
    qc <- readQc(system.file("..",
                             "tests",
                             "testthat",
                             "plasma_qc_report.xml",
                             package = "nmr.parser"))
  } else if (matrixType == "URI") {

    qc <- readQc(system.file("..",
                             "tests",
                             "testthat",
                             "urine_qc_report.xml",
                             package = "nmr.parser"))

    }
  qc <- qc$data

  tbl1 <- data.frame(testName = qc$tests$name,
                     testType = qc$tests$type,
                     testUnit = qc$tests$unit,
                     testRefMax = qc$tests$refMax,
                     testRefMin = qc$tests$refMin,
                     testDescription = NA)

  # fixing tests with same names
  if (matrixType == "SER") {

  fi <- grep(":", tbl1$testType)
  tbl1$testName[fi] <- paste0(tbl1$testName[fi],
                              sapply(tbl1$testType[fi], function(x) strsplit(x, ":")[[1]][2]))

  }

  if (withValue) {
    tbl1$testDescription <- qc$tests$comment
    tbl1 <- cbind(tbl1, value = qc$tests$value)
  }

  tbl2 <- data.frame(testName = qc$infos$name,
                     testType = NA,
                     testUnit = NA,
                     testRefMax = NA,
                     testRefMin = NA,
                     testDescription = NA)

  if (withValue) {
    tbl2$testDescription <- qc$infos$value
    tbl2 <- cbind(tbl2, value = NA)
  }

  return(rbind(tbl2, tbl1))
}
