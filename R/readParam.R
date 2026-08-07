#' look one parameter up in the lines of a bruker file
#'
#' Anchored on the declaration itself. grep(paste0(paramName, "=")) matched
#' the name anywhere in the line, so it read the first line that happened to
#' contain it: acqus files are ordered alphabetically, which put LOCSW ahead
#' of SW, DATE ahead of TE and CPDPRG ahead of RG. 21 of the 195 parameters
#' in inst/HB-COVID0001/10/acqus collide that way.
#'
#' Compared as a prefix rather than matched as a regexp, so that a name
#' containing . or + is not read as a pattern.
#'
#' @param txt - the lines of the file
#' @param paramName - the name of a single parameter
#' @param path - the file, for the diagnostic
#' @return the parsed value, or NULL when the parameter is not declared
#' @noRd
findParam <- function(txt, paramName, path) {

  # ##$NAME= for parameters, ##NAME= for the header fields
  idx <- which(startsWith(txt, paste0("##$", paramName, "=")) |
                 startsWith(txt, paste0("##", paramName, "=")))

  if (length(idx) == 0) {
    return(NULL)
  }

  if (length(idx) > 1) {
    cat(crayon::yellow("readParam param", paramName, "declared",
                       length(idx), "times, using the first:\n"))
    cat(crayon::blue(path, "\n"))
  }

  # everything after the first =, so that a value containing one survives
  # strsplit(line, "=")[[1]][2] used to cut it there
  value <- sub("^[^=]*=[[:blank:]]?", "", txt[idx[1]])

  if (grepl("<", value)) {
    return(gsub(" ", "", (gsub("<", "", (gsub(">", "", value))))))
  } else {
    return(as.numeric(value))
  }
}

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

      parameter <- findParam(txt, paramName, path)

      if (is.null(parameter)) {
        cat(crayon::yellow("readParam param", paramName, "not found or ... \n"))
        cat(crayon::yellow("readParam file is empty or corrupted:\n"))
        cat(crayon::blue(path, "\n"))
        return(NULL)
      }

      return(parameter)
    }

    # if param is length > 1 each one is looked up on its own and a miss is NA
    # in place, so that the result stays aligned with paramName. Testing the
    # whole file on paramName[1] used to discard the other params along with it
    parameters <- list()
    for (p in seq_along(paramName)) {

      prm <- findParam(txt, paramName[p], path)

      if (is.null(prm)) {
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
