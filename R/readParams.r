
readParams <- function(filePath) {
  if (file.exists(filePath)) {
    buf <- file(filePath, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)
    content <- list()
    counter <- 1 # line counter
    while (counter < length(txt)) {
      line <- txt[[counter]]
      if (grepl("^##END=", line)) {
        break
      }
      path <- strsplit(filePath, "/")[[1]]
      # get titles
      if (grepl("^##[A-Z]", line)) {
        param <- strsplit(line, "= ")[[1]]
        value <- param[2]
        cleanValue <- gsub("\\t", " ", value)
        cleanValue <- gsub("\\$\\$", "", cleanValue)
        cleanValue <- gsub("\\s+", " ", cleanValue)
        content <- c(content, list(c(path = path[length(path)],
                                     name = gsub("##", "", param[1]),
                                     value = cleanValue)))
      }
      # get audit info
      if (grepl("^\\$\\$\\s", line)) {
        param <- gsub("\\$\\$\\s", "", line)
        param <- gsub("\\s+", " ", param)
        if (grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]", param)) {
          date <- strsplit(param, " ")[[1]][1]
          time <- strsplit(param, " ")[[1]][2]
          timezone <- strsplit(param, " ")[[1]][3]
          instrument <- strsplit(param, " ")[[1]][4]
          content <- c(content, list(c(path = path[length(path)],
                                       name = "date",
                                       value = date)))
          content <- c(content, list(c(path = path[length(path)],
                                       name = "time",
                                       value = time)))
          content <- c(content, list(c(path = path[length(path)],
                                       name = "timezone",
                                       value = timezone)))
          content <- c(content, list(c(path = path[length(path)],
                                       name = "instrument",
                                       value = instrument)))
        }

      }
      # get params
      if (grepl("^##\\$", line)) {
        param <- strsplit(line, "= ")[[1]]
        value <- param[2]
        # look for vectors
        if (grepl("\\([0-9]+..[0-9]+\\)", value)) {
          counter <- counter + 1
          vect <- txt[[counter]]
          value <- strsplit(vect, " ")
          for (i in 1:length(value[[1]])) {
            content <- c(content, list(c(path = path[length(path)],
                                         name = paste0(gsub("##\\$", "", param[1]), "_", i - 1),
                                         value = value[[1]][i])))
          }
        } else {
          cleanValue <- gsub("[<->]", "", value)
          content <- c(content, list(c(path = path[length(path)],
                                       name = gsub("##\\$", "", param[1]),
                                       value = cleanValue)))
        }
      }
      counter <- counter + 1
    }
    ret <- data.frame(do.call(rbind, content))
    fi <- sapply(ret$value, function(x) identical(x, character(0)))
    ret$value[fi] <- NA

    return(data.frame(path = unlist(ret$path),
                      name = unlist(ret$name),
                      value = unlist(ret$value)))
    # return(ret)
  } else {
    cat(crayon::yellow("fusion::readParams >>", filePath, " file not found\n"))
  }
}
