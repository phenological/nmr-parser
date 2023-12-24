#' extract a parameter from a bruker file (procs or acqus)
#'
#' @param path - the path to the expName folder
#' @param paramName - the name of the parameter to read
#' @return the parameter
#'
#' @export
readParam <- function(path, paramName){
  if (file.exists(path)) {
  buf <- file(path, open = "r")
  txt <- readLines(buf, n = -1, warn = FALSE)
  close(buf)
  parameter <- strsplit(txt[grep(paste0(paramName, "="), txt)], "=")[[1]][2]
  if (grepl("<", parameter) == TRUE) {
    return(gsub(" ", "", (gsub("<", "", (gsub(">", "", parameter))))))
  } else {
    return(as.numeric(parameter))
  }
  } else {
    cat(crayon::yellow("readParam file does not exist\n"))
    return(NULL)
  }
}
