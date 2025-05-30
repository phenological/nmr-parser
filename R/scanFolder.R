#' function to scan a folder and get a list of paths to parse
#' @param folder - the root folder(s) to scan
#' @param options - options to skip the prompt
#' @returns a list of paths
#' @importFrom data.table data.table
#' @importFrom stats setNames
#' @importFrom utils stack menu
#' @export

scanFolder <- function(folder, options = list()) {

  EXP <- PULPROG <- ""
  if ("EXP" %in% names(options)) {
    EXP <- options$EXP
  }

  if ("PULPROG" %in% names(options)) {
    PULPROG <- options$PULPROG
  }

  acqusFiles <- list.files(folder,
                           pattern = "^acqus$",
                           recursive = TRUE,
                           full.names = TRUE)

  # filtering acqus.safe and similar
  acqusFiles <- grep("/acqus$", acqusFiles, value = TRUE)

  # filtering odd expnos
  fi <- grepl("99999/acqus|98888/acqus", acqusFiles)
  acqusFiles <- acqusFiles[!fi]

  # remove trailing slash
  acqusFiles <- gsub("//", "/", acqusFiles)


  expList <- list()
  for (i in 1:length(acqusFiles)) {
    prms <- readParam(acqusFiles[i], c("EXP", "PULPROG", "USERA2"))
    cat("Scanning:", i, "/", length(acqusFiles), "\r")

    expList[[i]] <- c(file = acqusFiles[i],
                      EXP = cleanNames(prms[[1]]),
                      PULPROG = cleanNames(prms[[2]]),
                      USERA2 = prms[[3]])
  }
  expList <- data.table(do.call("rbind", expList))





  if (EXP == "" & PULPROG == "") {
    res <- stack(table(paste0(expList$EXP, "@", expList$PULPROG)))
    res <- setNames(res, c("count", "EXP@PULPROG"))

    question <- paste0("Choose what to parse.")
    choice <- menu(choices = paste0(res$`EXP@PULPROG`, " (", res$count, ")"),
                   title = question)
    choice <- strsplit(as.character(res$`EXP@PULPROG`[choice]), "@")
    fi <- grepl(choice[[1]][1], expList$EXP) &
      grepl(choice[[1]][2], expList$PULPROG)
    expList <- expList[fi,]
  } else if (EXP == "ignore" & PULPROG == "") {
    res <- stack(table(expList$PULPROG))
    res <- setNames(res, c("count", "PULPROG"))

    question <- paste0("Choose what to parse.")
    choice <- menu(choices = paste0(res$PULPROG, " (", res$count, ")"),
                   title = question)
    choice <- as.character(res$PULPROG[choice])

    fi <- grepl(choice, expList$PULPROG)
    expList <- expList[fi,]
  }
  else {
    fi <- grepl(EXP, expList$EXP) &
      grepl(PULPROG, expList$PULPROG)
    expList <- expList[fi,]
  }


  res <- stack(table(paste0(expList$EXP, "@", expList$PULPROG)))
  for (i in 1:nrow(res)) {
    message(cat(crayon::blue("scanFolder >> ",
                             paste0(res$ind[i],
                                    ": ",
                                    res$values[i]))))
  }
  expList$file <- gsub("/acqus", "", expList$file)
  return(expList)
}


