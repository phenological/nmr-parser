
#' Get a data frame with the QC table
#' @param matrixType A string with the matrix type. It can be "SER" or "URI"
#' @param withValue A boolean to include the value column
#' @export
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
                     testUnit = qc$tests$unit,
                     testRefMax = qc$tests$refMax,
                     testRefMin = qc$tests$refMin,
                     testDescription = qc$tests$type)

  # fixing tests with same names
  if (matrixType == "SER") {

  fi <- grep(":", tbl1$testDescription)
  tbl1$testName[fi] <- paste0(tbl1$testName[fi],
                              sapply(tbl1$testDescription[fi], function(x) strsplit(x, ":")[[1]][2]))
  tbl1$testDescription[fi] <- sapply(tbl1$testDescription[fi], function(x) gsub(" :", "", x))
  tbl1$testDescription[fi] <- sapply(tbl1$testDescription[fi], function(x) gsub("\\.", "", x))
  }

  if (withValue) {
    tbl1 <- cbind(tbl1, status = qc$tests$comment)
    tbl1 <- cbind(tbl1, value = qc$tests$value)
  }

  tbl2 <- data.frame(testName = qc$infos$name,
                     testUnit = NA,
                     testRefMax = NA,
                     testRefMin = NA,
                     testDescription = NA)

  if (withValue) {
    tbl2 <- cbind(tbl2, value = qc$infos$value)
    tbl2 <- cbind(tbl2, status = qc$infos$comment)
  }

  return(rbind(tbl2, tbl1))
}
