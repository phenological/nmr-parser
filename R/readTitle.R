#' extract title from a bruker folder
#'
#' @param file - the path to the expName folder
#' @return the title
#'
#' @export

readTitle <- function(file){
  if (file.exists(file)) {
    buf <- file(file, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)
    content <- list()
    for (i in 1:length(txt)) {
      if (txt[i] != "") {
        cnt <- gsub("\\s*$", "", txt[i])
        content <- c(content, list(c(path = "title", name = "title", value = cnt)))
      }
    }
    content <- sapply(content, function(x) unname(x[3]))
    cnt <- paste0(content, collapse = "\n")
    return(list(path = "title", name = "title", value = cnt))
  } else {
    cat(crayon::yellow("readTitle >>", file, "not found\n"))
  }
}

