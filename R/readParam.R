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
    
    # if param is length 1, a miss is NULL: callers such as readSpectrum()
    # test for that before going on
    if (length(paramName) == 1) {

      idx <- grep(paste0(paramName, "="), txt)

      if (length(idx) == 0) {
        cat(crayon::yellow("readParam param", paramName, "not found or ... \n"))
        cat(crayon::yellow("readParam file is empty or corrupted:\n"))
        cat(crayon::blue(path, "\n"))
        return(NULL)
      }

      parameter <- strsplit(txt[idx], "=")[[1]][2]

      if (grepl("<", parameter) == TRUE) {
        return(gsub(" ", "", (gsub("<", "", (gsub(">", "", parameter))))))
      } else {
        return(as.numeric(parameter))
      }
    }

    # if param is length > 1 each one is looked up on its own and a miss is NA
    # in place, so that the result stays aligned with paramName. Testing the
    # whole file on paramName[1] used to discard the other params along with it
    parameters <- list()
    for (p in seq_along(paramName)) {

      idx <- grep(paste0(paramName[p], "="), txt)

      if (length(idx) > 0) {
        prm <- strsplit(txt[idx], "=")[[1]][2]

        if (grepl("<", prm) == TRUE) {
          prm <- gsub(" ", "", (gsub("<", "", (gsub(">", "", prm)))))
        } else {
          prm <- as.numeric(prm)
        }

      } else {
        cat(crayon::yellow("readParam param", paramName[p], "not found:\n"))
        cat(crayon::blue(path, "\n"))
        prm <- NA
      }

      parameters[[p]] <- prm
    }

    return(parameters)

  } else {
    cat(crayon::yellow("readParam file does not exist\n"))
    return(NULL)
  }
}
