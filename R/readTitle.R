#' extract title from a bruker folder
#'
#' @param filePath - the path to the expName folder
#' @return the title
#'
#' @export

readTitle <- function(filePath){
  if (file.exists(filePath)) {
    buf <- file(filePath, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)
    content <- list()
    for (i in 1:length(txt)) {
      if (txt[i] != "") {
        cnt <- gsub("\\s*$", "", txt[i])
        content <- c(content, list(c(path = "title", name = "title", value = cnt)))
      }
    }
    res <- do.call(rbind, content)
    return(res)
  } else {
    return("no title")
  }
}

