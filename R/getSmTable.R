#' get names and description of brxsm plasma reports
#' @return a data.frame with information
#' @importFrom utils data
#' @importFrom data.table setDT
#' @export
getSmTable <- function() {
  sm <- get0("brxsm_pla", envir = asNamespace("nmr.parser"))
  sm[["data"]]$range <- paste0(sm$data$refMin, " - ", sm$data$refMax,
                       " (", sm$data$refUnit, ")")
  sm[["data"]]$range <- gsub("- -", "<", sm[["data"]]$range)

  setDT(sm$data)
  cols <- c("name", "rawConcUnit", "refMin", "refMax", "refUnit", "range")
  cols %in% names(sm$data)
  sm$data <- sm$data[, cols, with = FALSE]

  names(sm$data) <- c("Compound",
                  "Unit",
                  "Max Value (ref.)",
                  "Min Value (ref.)",
                  "Reference Unit",
                  "Reference Range [Unit]")


  rownames(sm$data) <- c(1:nrow(sm$data))



  return(sm$data)

}

