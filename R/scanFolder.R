#' function to scan a folder and get a list of paths to parse
#' @param folder - the root folder to scan
#' @param options - options to skip the prompt
#' @returns a list of paths
#' @importFrom data.table data.table
#' @importFrom stats setNames
#' @importFrom utils stack menu

scanFolder <- function(folder, options = list()) {

  EXP <- PULPROG <- ""
  if ("EXP" %in% names(options)) {
    EXP <- options$EXP
  }

  if ("PULPROG" %in% names(options)) {
    PULPROG <- options$PULPROG
  }

  acqusFiles <- list.files(folder,
                           pattern = "acqus",
                           recursive = TRUE,
                           full.names = TRUE)

  # filtering odd expnos
  fi <- grepl("99999/acqus|98888/acqus", acqusFiles)
  acqusFiles <- acqusFiles[!fi]

  expList <- lapply(acqusFiles,
                    function(a) c(file = a,
                                  EXP = readParam(a, "EXP"),
                                  PULPROG = readParam(a, "PULPROG")))
  expList <- data.table(do.call("rbind", expList))

  res <- stack(table(paste0(expList$EXP, "@", expList$PULPROG)))
  res <- setNames(res, c("count", "EXP@PULPROG"))

  if (EXP == "" & PULPROG == "") {
    question <- paste0("Choose what to parse.")
    choice <- menu(choices = paste0(res$`EXP@PULPROG`, " (", res$count, ")"),
                   title = question)
    choice <- strsplit(as.character(res$`EXP@PULPROG`[choice]), "@")
    fi <- grepl(choice[[1]][1], expList$EXP) &
      grepl(choice[[1]][2], expList$PULPROG)
    expList <- expList[fi,]
  } else {
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

  return(as.list(gsub("/acqus", "", expList$file)))
}


