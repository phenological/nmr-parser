
#' This function is used to get the table of PACS data from the test data
#' @export
getPacsTable <- function() {
  pacs <- readPacs(system.file("..",
                               "tests",
                               "testthat",
                               "plasma_pacs_report.xml",
                               package = "nmr.parser"))

  tbl <- pacs$data
  # name conc_v concUnit_v refMax refMin refUnit
  names(tbl) <- c("name", "conc", "unit", "refMax", "refMin", "refUnit")

  tbl <- tbl[, c("name", "unit", "refMax", "refMin", "refUnit")]

  return(tbl)
}
