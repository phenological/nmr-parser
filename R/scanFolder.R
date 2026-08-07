#' ask the user to pick one of the experiment types found
#'
#' menu() only works at a prompt: run from Rscript it returns 0 without
#' asking, which used to be read as a selection and fail on a subscript out
#' of bounds. Zero is also what the user gets for choosing 0 to exit.
#'
#' @param choices - the labels to offer
#' @param what - what is being chosen, for the message
#' @return the index chosen, or NULL when there is no choice to be had
#' @noRd
askFor <- function(choices, what) {

  if (!interactive()) {
    stop("scanFolder >> cannot prompt for ", what, " outside an interactive ",
         "session. Pass it in, e.g. ",
         "scanFolder(folder, options = list(EXP = \"...\", PULPROG = \"...\"))",
         call. = FALSE)
  }

  choice <- menu(choices = choices, title = paste("Choose what to parse:", what))

  if (choice == 0) {
    message(crayon::yellow("scanFolder >> nothing selected"))
    return(NULL)
  }

  return(choice)
}

#' function to scan a folder and get a list of paths to parse
#' @param folder - the root folder(s) to scan
#' @param options - options to skip the prompt. EXP and PULPROG are matched
#'   as patterns, so "noesy" selects noesygppr1d. EXP = "ignore" prompts for
#'   the PULPROG alone
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

  # nothing to scan. Falling through would reach menu() with no choices,
  # which errors non-interactively and offers an empty list otherwise
  if (length(acqusFiles) == 0) {
    message(crayon::yellow(paste("scanFolder >> no acqus found under",
                                 paste(folder, collapse = ", "))))
    return(data.table(file = character(0),
                      EXP = character(0),
                      PULPROG = character(0),
                      USERA2 = character(0)))
  }

  expList <- list()
  for (i in seq_along(acqusFiles)) {
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

    choice <- askFor(paste0(res$`EXP@PULPROG`, " (", res$count, ")"),
                     "EXP and PULPROG")
    if (is.null(choice)) {
      return(expList[0, ])
    }

    # the choice comes from the data, so it is compared, not matched: an
    # EXP or PULPROG carrying a regexp character would select the wrong rows
    choice <- strsplit(as.character(res$`EXP@PULPROG`[choice]), "@")[[1]]
    fi <- expList$EXP == choice[1] & expList$PULPROG == choice[2]
    expList <- expList[fi,]
  } else if (EXP == "ignore" & PULPROG == "") {
    res <- stack(table(expList$PULPROG))
    res <- setNames(res, c("count", "PULPROG"))

    choice <- askFor(paste0(res$PULPROG, " (", res$count, ")"), "PULPROG")
    if (is.null(choice)) {
      return(expList[0, ])
    }

    fi <- expList$PULPROG == as.character(res$PULPROG[choice])
    expList <- expList[fi,]
  }
  else {
    # EXP and PULPROG given in options stay patterns, so that a caller can
    # ask for "noesy" and get noesygppr1d
    fi <- grepl(EXP, expList$EXP) &
      grepl(PULPROG, expList$PULPROG)
    expList <- expList[fi,]
  }


  res <- stack(table(paste0(expList$EXP, "@", expList$PULPROG)))
  for (i in seq_len(nrow(res))) {
    message(cat(crayon::blue("scanFolder >> ",
                             paste0(res$ind[i],
                                    ": ",
                                    res$values[i]))))
  }
  expList$file <- gsub("/acqus", "", expList$file)
  return(expList)
}


