#' report how many expnos yielded one kind of data
#'
#' One line per category, in one shape, so that a run reads as a tally
#' rather than as nine differently worded sentences. The counts used to be
#' phrased four ways ("0 found pacs", "1 found spectrum(a)") and gave the
#' number found without the number asked for, which is the interesting part
#' when a run is meant to cover a rack.
#'
#' message() rather than message(cat(...)): cat() prints to stdout and
#' returns NULL, so the text escaped suppressMessages() and every call also
#' emitted an empty line to stderr.
#'
#' @param what - the category
#' @param found - how many expnos produced something
#' @param asked - how many were looked at
#' @return nothing, called for the message
#' @noRd
reportFound <- function(what, found, asked) {

  line <- paste0("readExperiment >> ", what, ": ", found, "/", asked)

  if (found == 0) {
    message(crayon::yellow(line))
  } else {
    message(crayon::blue(line))
  }
}

#' read one parameter file from every expno and stack the results
#'
#' The acqus and the procs blocks were the same code twice over, differing
#' only in the file to read and the prefix to put on the columns.
#'
#' Only the columns every expno has in common survive, since the parameter
#' sets differ between acquisition types.
#'
#' @param expname - the expno folders
#' @param relPath - the file to read, relative to the expno
#' @param prefix - what to call the value columns
#' @return a data.table, empty when nothing could be read
#' @noRd
readParamTable <- function(expname, relPath, prefix) {

  lst <- list()

  for (l in seq_along(expname)) {
    # a running count is worth showing over a rack, not over one expno
    if (length(expname) > 1) {
      cat("Reading:", l, "/", length(expname), "\r")
    }

    path <- file.path(expname[[l]], relPath)

    if (file.exists(path)) {
      parms <- readParams(path)

      # we check for empty files
      if (!is.null(parms)) {
        parms$path <- expname[[l]]
        lst[[length(lst) + 1]] <- reshape(parms,
                                          idvar = "path",
                                          timevar = "name",
                                          direction = "wide")
      }
    }
  }

  if (length(lst) == 0) {
    return(data.table())
  }

  common_columns <- Reduce(intersect, lapply(lst, names))
  res <- data.table(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
  colnames(res) <- gsub("value.", paste0(prefix, "."), colnames(res))

  return(res)
}

#' find the eretic factor an expno carries
#'
#' Two file layouts, and the choice between them was written out twice, once
#' in the eretic block and once in the spec block.
#'
#' @param expno - the folder to look in
#' @return the factor, or NULL when the folder carries neither file
#' @noRd
readEreticFactor <- function(expno) {

  quantFactor <- file.path(expno, "QuantFactorSample.xml")
  ereticFile <- file.path(expno, "pdata", "1", "eretic_file.xml")

  if (file.exists(quantFactor)) {
    return(readEretic(quantFactor)$ereticFactor)
  }

  # in the case of F80 we use a different parser
  if (file.exists(ereticFile)) {
    return(readEreticF80(ereticFile)$samOneMolInt)
  }

  return(NULL)
}

#' read experiment from a bruker folder (expno)
#'
#' @param expname - a path (or list of) to the expNo folder(s)
#' @param opts - object with processing parameters, if TRUE reads from the file
#' what - choose what to read (acqus, procs, qc, title, eretic, spec, lipo, quant, pacs, all)
#' @return a list with all read elements
#'
#' @export
#' @importFrom stats reshape
#' @importFrom data.table data.table setDT
#' @importFrom utils modifyList
# curl -X 'GET' $ROLDX_URL/link\?runName\=EXTr01 | jq '.list | .noesygppr1d | .[0] | keys'
readExperiment <- function(expname, opts = NULL) {

  defaultOptions = list(what = c("acqus",
                          "procs",
                          "qc",
                          "title",
                          "eretic",
                          "spec",
                          "lipo",
                          "quant",
                          "pacs",
                          "all",
                          "specOnly"),
                 procno = 1,
                 specOpts = list(uncalibrate = FALSE,
                                 fromTo = c(-0.1, 10),
                                 length.out = 44079,
                                 # one set of notes per spectrum drowns the
                                 # run, the tally below says the same thing
                                 verbose = FALSE))

  # Merge provided options with defaults
  if (is.null(opts)) {
    opts <- defaultOptions
  } else {
    # Handle nested specOpts separately
    if ("specOpts" %in% names(opts) && !is.null(opts$specOpts)) {
      opts$specOpts <- modifyList(defaultOptions$specOpts, opts$specOpts)
    }
    opts <- modifyList(defaultOptions, opts)
  }

  name <- path <- conc_v <- concUnit_v <- refMax <- refMin <- NULL
  id <- value <- unit <- rawConc <- NULL

  what <- opts$what

  # a typo in what used to read nothing and say nothing about why
  unknown <- setdiff(what, defaultOptions$what)
  if (length(unknown) > 0) {
    warning(crayon::red(paste0("readExperiment >> unknown what: ",
                               paste(unknown, collapse = ", "),
                               ". Known: ",
                               paste(defaultOptions$what, collapse = ", "))),
            call. = FALSE)
  }

  if (is.character(expname)) {
    expname <- as.list(expname)
  }

  # nor did anything say that the folders were not there
  missing <- !vapply(expname, dir.exists, logical(1))
  if (any(missing)) {
    message(crayon::yellow(paste0("readExperiment >> ", sum(missing), "/",
                                  length(expname), " expno folders not found, first: ",
                                  expname[missing][[1]])))
  }

  res <- list()

  if ("acqus" %in% what | "all" %in% what) {
    res$acqus <- readParamTable(expname, "acqus", "acqus")

    reportFound("acqus", nrow(res$acqus), length(expname))
  }

  if ("procs" %in% what | "all" %in% what) {
    res$procs <- readParamTable(expname, file.path("pdata", "1", "procs"), "procs")

    reportFound("procs", nrow(res$procs), length(expname))
  }

  if ("qc" %in% what | "all" %in% what) {
    lst <- list()
    for (l in seq_along(expname)) {
      if (length(expname) > 1) {
        cat("Reading:", l, "/", length(expname), "\r")
      }
      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = "qc_report.*\\.xml$")
      if (any(grepl("1_1_0.xml", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0.xml", path)]
      }
      # there is only one report per pdata, so anything left over is a version
      # variant we do not know about. dir() sorts, so the last one is the
      # latest. readQc() would error on a vector of paths.
      if (length(path) > 1) {
        path <- path[length(path)]
      }
      if (length(path)>0) {
        qc <- readQc(path)$data
      } else {
        qc <- NULL
      }
      if (!is.null(qc)) {
        qc$path <- expname[[l]]
        lst[[l]] <- qc
      }
    }

    res$qc <- data.table(do.call("rbind", lst))
    reportFound("qc", nrow(res$qc), length(expname))
  }

  if ("title" %in% what | "all" %in% what) {
    lst <- list()
    for (l in seq_along(expname)) {

      path <- file.path(expname[[l]], "pdata", "1", "title")
      if (file.exists(path)) {
        lst[[l]] <- c(path = expname[[l]], title = readTitle(path)[[3]])
      }
    }

    res$title <- data.table(do.call("rbind", lst))

    reportFound("title", nrow(res$title), length(expname))
  }

  if ("eretic" %in% what | "all" %in% what) {
    lst <- list()

    for (l in seq_along(expname)) {

      # this block reports what the expno itself carries, the spec block
      # below looks in the expno ending in 0 instead
      ereticFactor <- readEreticFactor(expname[[l]])

      if (!is.null(ereticFactor)) {
        lst[[l]] <- c(path = expname[[l]], ereticFactor = ereticFactor)
      }
    }


    res$eretic <- data.table(do.call("rbind", lst))
    reportFound("eretic", nrow(res$eretic), length(expname))


  }

  ifelse("procno" %in% names(opts), procno <- opts$procno, procno <- 1)

  if ("spec" %in% what | "all" %in% what | "specOnly" %in% what) {
    lst <- list()
    for (l in seq_along(expname)) {

      if("specOpts" %in% names(opts)) {
        specOpts <- opts$specOpts
      } else {
        specOpts = defaultOptions$specOpts
      }

      # we always look in the first folder (ANPC folder structure)
      # this is pretty safe but may be wrong for some reruns
      # replace the last digit of the expno, not the last character of the
      # whole path, which a trailing slash turns into a different expno
      expno <- basename(expname[[l]])
      ereticPath <- file.path(dirname(expname[[l]]),
                              paste0(substr(expno, 1, nchar(expno) - 1), "0"))

      if ("eretic" %in% names(specOpts)) {

        # we first look for eretic in the options (so we can override it if necessary)
        # in cases such as ileum spiking that has a wrong folder structure
        ereticFactor <- specOpts$eretic
      } else {
        # we look for eretic in the rexpno + 0 folder
        # to correct for pgpe and other type of experiments that need correction
        ereticFactor <- readEreticFactor(ereticPath)

        # if nothing was found we set the factor to 1 and display a warning
        if (is.null(ereticFactor)) {
          ereticFactor <- 1
        }
      }

      specOpts$eretic <- ereticFactor
      spec <- readSpectrum(expname[[l]], procno, procs = TRUE, options = specOpts)

      if (!is.null(spec)) {
        lst[[l]] <- list(path = expname[[l]], spec = spec)

        if (ereticFactor == 1) {
          warning(crayon::red("readExperiment >> ereticFactor set to 1:", ereticPath))
        }

      }
    }

    res$spec <- data.table(do.call("rbind", lst))
    reportFound("spec", nrow(res$spec), length(expname))
  }

  if ("lipo" %in% what | "all" %in% what) {
    lst <- list()
    for (l in seq_along(expname)) {

      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*lipo.*\\.xml$")
      if(any(grepl("1_1_0", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0", path)]
      }
      # only one report per pdata, so keep the latest of any leftover version
      # variants. readLipo() would error on a vector of paths.
      if (length(path) > 1) {
        path <- path[length(path)]
      }

    if (length(path)>0) {
        lipoproteins <- readLipo(path)
        if (!is.null(lipoproteins)) {
          lipoproteins$data$path <- expname[[l]]
          lst[[l]] <- lipoproteins
        }
      }
    }

    res$lipo <- data.table(do.call("rbind", lst))
    if (nrow(res$lipo) == 0) {
      reportFound("lipo", 0, length(expname))
    } else {
      lipo <- lapply(res$lipo$data,
                     function(x) reshape(setDT(x)[, .(id, path, value, unit, refMax, refMin),],
                                         idvar = "path",
                                         timevar = "id",
                                         direction = "wide"))
      lipo <- do.call("rbind", lipo)
      res$lipo <- lipo
      reportFound("lipo", nrow(res$lipo), length(expname))
    }
  }

  if ("pacs" %in% what | "all" %in% what) {
    lst <- list()
    for (l in seq_along(expname)) {
      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*pacs.*\\.xml$")
      if(any(grepl("1_1_0", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0", path)]
      }
      # only one report per pdata, so keep the latest of any leftover version
      # variants. readPacs() would error on a vector of paths.
      if (length(path) > 1) {
        path <- path[length(path)]
      }

      if (length(path)>0) {
        pacs <- readPacs(path)
      } else {
        pacs <- NULL
      }
      if (!is.null(pacs)) {
        pacs$data$path <- expname[[l]]
        lst[[l]] <- pacs
      }
    }

    res$pacs <- data.table(do.call("rbind", lst))

    if (nrow(res$pacs) == 0) {
      reportFound("pacs", 0, length(expname))
    } else {
      pacs <- lapply(res$pacs$data,
                     function(x) reshape(setDT(x)[, .(name, path, conc_v, concUnit_v, refMax, refMin),],
                                         idvar = "path",
                                         timevar = "name",
                                         direction = "wide"))
      pacs <- do.call("rbind", pacs)
      colnames(pacs) <- gsub("conc_v.", "value.", colnames(pacs))
      colnames(pacs) <- gsub("concUnit_v.", "unit.", colnames(pacs))
      res$pacs <- pacs
      reportFound("pacs", nrow(res$pacs), length(expname))
    }
  }

  if ("quant" %in% what | "all" %in% what) {
    # Define version priority (highest first)
    # plasma_quant_report_ver_1_0.xml sits below the plain report rather than
    # above it, where the urine ordering would put it: an expno holding both
    # keeps reading the one it always read, and only an expno holding just
    # the extended report changes, from no quant data at all to its data.
    quantPriority <- c("plasma_quant_report_2_1_0.xml","plasma_quant_report.xml",
                       "plasma_quant_report_ver_1_0.xml",
                       "urine_quant_report_e_1_2_0.xml",  "urine_quant_report_e_ver_1_0.xml",
                       "urine_quant_report_e.xml",
                       "urine_quant_report_b_ver_1_0.xml","urine_quant_report_b.xml",
                       "urine_quant_report_ne_ver_1_0.xml","urine_quant_report_ne.xml")

    lst <- list()
    for (l in seq_along(expname)) {
      folderPath <- file.path(expname[[l]], "pdata", "1")
      # Find all matching xml files
      path <- dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*quant.*\\.xml$")

      # Pick the highest-priority match, on the file name rather than on the
      # priority entry read as a regexp, where the . before xml is a wildcard
      chosen <- NULL
      for (ver in quantPriority) {
        matches <- path[basename(path) == ver]
        if (length(matches) > 0) {
          chosen <- matches
          break
        }
      }
      
      # Read the match, keeping the latest when the same report shows up twice
      # (dropping the sample here would lose it silently)
      if (!is.null(chosen)) {
        quant <- readQuant(chosen[length(chosen)])
      } else {
        quant <- NULL
      }

      if (!is.null(quant)) {
        quant$data$path <- expname[[l]]
        lst[[l]] <- quant
      }
    }

    res$quant <- data.table(do.call("rbind", lst))
    if (nrow(res$quant) == 0) {
      reportFound("quant", 0, length(expname))
    } else {
      quant <- lapply(res$quant$data,
                      function(x) reshape(setDT(x)[, .(name, path, rawConc, concUnit_v, refMax, refMin),],
                                          idvar = "path",
                                          timevar = "name",
                                          direction = "wide"))
      quant <- do.call("rbind", quant)
      colnames(quant) <- gsub("rawConc.", "value.", colnames(quant))
      colnames(quant) <- gsub("concUnit_v.", "unit.", colnames(quant))
      res$quant <- quant
      reportFound("quant", nrow(res$quant), length(expname))
    }
  }

  return(res)
}


# path <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/11"
# exp <- readExperiment(path)
