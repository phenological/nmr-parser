#' extract a parameter from a bruker file (procs or acqus)
#'
#' @param file - the path to the expName folder
#' @return the parameter
#'
#' @export
readParams <- function(file) {
  if (file.exists(file)) {
    buf <- file(file, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)

    # test if file is empty
    if (length(txt) == 0) {
      cat(crayon::yellow("readParams >>", file, " file is empty\n"))
      return(NULL)
    }

    # test for AMIX files
    if (txt[1] == "A000") {
      cat(crayon::yellow("readParams >>", file, " file is AMIX\n"))
      return(NULL)
    }

    fileName <- basename(file)
    content <- list()
    counter <- 1 # line counter
    incr <- 1
    while (counter <= length(txt)) {


      line <- txt[[counter]]


      if (grepl("^##END=", line)) {
        break
      }


      # get titles
      if (grepl("^##[A-Z]", line)) {

        param <- strsplit(line, "= ")[[1]]
        value <- param[2]
        cleanValue <- gsub("\\t", " ", value)
        cleanValue <- gsub("\\$\\$", "", cleanValue)
        cleanValue <- gsub("\\s+", " ", cleanValue)
        content[[incr]] <- list(path = fileName,
                                name = gsub("##", "", param[1]),
                                value = cleanValue)
      } else

        # get audit info
        if (grepl("^\\$\\$\\s", line)) {
          date <- time <- timezone <- instrument <- dpath <- NULL
          param <- gsub("\\$\\$\\s", "", line)
          param <- gsub("\\s+", " ", param)

          # XwinNMR
          if (grepl("^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]", param)) {

            date <- strsplit(param, " ")[[1]][1]
            time <- strsplit(param, " ")[[1]][2]
            timezone <- strsplit(param, " ")[[1]][3]
            instrument <- cleanNames(strsplit(param, " ")[[1]][4])

            # Topspin
          } else if (grepl("^[A-Z][a-z][a-z]\\s[A-Z][a-z][a-z]\\s[0-9]", param)) {

            param <- gsub("\\s+", " ", param)
            date <- paste0(strsplit(param, " ")[[1]][c(1:3,5)], collapse = " ")
            time <- strsplit(param, " ")[[1]][4]
            timezone <- paste0(strsplit(param, " ")[[1]][c(6,7)], collapse = " ")
            instrument <- cleanNames(strsplit(param, " ")[[1]][8])


          } else if (grepl("^[A-Z]:", param) | grepl("^/u", param)){
            dpath <- param

          }

          if (!is.null(date) & !is.null(time) & !is.null(timezone) & !is.null(instrument)) {
            content[[incr]] <- list(path = fileName,
                                    name = "instrumentDate",
                                    value = date)
            incr <- incr + 1
            content[[incr]] <- list(path = fileName,
                                    name = "instrumentTime",
                                    value = time)
            incr <- incr + 1
            content[[incr]] <- list(path = fileName,
                                    name = "instrumentTimeZone",
                                    value = timezone)
            incr <- incr + 1
            content[[incr]] <- list(path = fileName,
                                    name = "instrument",
                                    value = instrument)
          }

          if (!is.null(dpath) ) {
            content[[incr]] <- list(path = fileName,
                                    name = "dpath",
                                    value = dpath)
          }

        } else

          # get params
          if (grepl("^##\\$", line)) {
            param <- strsplit(line, "= ")[[1]]
            value <- param[2]


            # looking for vectors
            if (grepl("\\([0-9]+..[0-9]+\\)", value)) {
              # the values sit on the next line, which a truncated file lacks
              if (counter == length(txt)) {
                break
              }
              counter <- counter + 1
              vect <- txt[[counter]]
              value <- strsplit(vect, " ")
              for (i in seq_along(value[[1]])) {
                content[[incr]] <- list(path = fileName,
                                        name = paste0(gsub("##\\$", "", param[1]), "_", i - 1),
                                        value = value[[1]][i])

                incr <- incr + 1

              }
            } else {
              # strip the < > Bruker wraps strings in. [<->] would be a range
              # from < to >, which also takes = with it
              cleanValue <- gsub("[<>]", "", value)
              content[[incr]] <- list(path = fileName,
                                      name = gsub("##\\$", "", param[1]),
                                      value = cleanValue)
            }
          }


      counter <- counter + 1
      incr <- incr + 1
    }



    ret <- data.frame(do.call(rbind, content))
    fi <- sapply(ret$value, function(x) identical(x, character(0)))
    ret$value[fi] <- NA


    return(data.frame(path = unlist(ret$path),
                      name = unlist(ret$name),
                      value = unlist(ret$value)))
    # return(ret)
  } else {
    cat(crayon::yellow("readParams >>", file, " file not found\n"))
  }
}
