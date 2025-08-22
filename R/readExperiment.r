#' read experiment from a bruker folder (expno)
#'
#' @param expname - a path (or list of) to the expNo folder(s)
#' @param opts - object with processing parameters, if TRUE reads from the file
#' what - choose what to read (acqus, procs, qc, title, eretic, spec, lipo, quant, pacs, all)
#' @return a list with all read elements
#'
#' @export
#' @importFrom stats reshape
#' @importFrom data.table setDT
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
                                 length.out = 44079))

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

  if (is.character(expname)) {
    expname <- as.list(expname)
  }

  res <- list()

  if ("acqus" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {
      cat("Reading:", l, "/", length(expname), "\r")

      path <- file.path(expname[[l]], "acqus")
      if (file.exists(path)) {
        parms <- readParams(path)

        # we check for empty files
        if (!is.null(parms)) {
          parms$path <- expname[[l]]
          lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
        } else {
          lst[[l]] <- NULL
        }
      } else {
        lst[[l]] <- NULL
      }
    }

    # we remove NULL elements from lst
    if (length(lst) > 0) {
      lst <- lst[sapply(lst, function(x) !is.null(x))]
    }

    common_columns <- Reduce(intersect, lapply(lst, names))
    res$acqus <- data.table(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
    colnames(res$acqus) <- gsub("value.", "acqus.", colnames(res$acqus))

    if (nrow(res$acqus) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found acqus params\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$acqus),
                               "found acqus params\n")))
    }
  }

  if ("procs" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {
      cat("Reading:", l, "/", length(expname), "\r")

      path <- file.path(expname[[l]], "pdata", "1", "procs")
      if (file.exists(path)) {
        parms <- readParams(path)

        # we check for empty files
        if (!is.null(parms)) {
          parms$path <- expname[[l]]
          lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
        } else {
          lst[[l]] <- NULL
        }

      } else {
        lst[[l]] <- NULL
      }
    }

    # we remove NULL elements from lst
    if (length(lst) > 0) {
      lst <- lst[sapply(lst, function(x) !is.null(x))]
    }

    common_columns <- Reduce(intersect, lapply(lst, names))
    res$procs <- data.table(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
    colnames(res$procs) <- gsub("value.", "procs.", colnames(res$procs))

    if (nrow(res$procs) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found procs\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$procs),
                               "found procs params\n")))
    }
  }

  if ("qc" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {
      cat("Reading:", l, "/", length(expname), "\r")
      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = "qc_report.*\\.xml$")
      if (any(grepl("1_1_0.xml", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0.xml", path)]
      }
      # path_serum <- file.path(expname[[l]], "pdata", "1", "plasma_qc_report.xml")
      # path_urine <- file.path(expname[[l]], "pdata", "1", "urine_qc_report.xml")
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
    if (nrow(res$qc) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found qc\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>", nrow(res$qc), "found qc\n")))
    }
  }

  if ("title" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {

      path <- file.path(expname[[l]], "pdata", "1", "title")
      if (file.exists(path)) {
        lst[[l]] <- c(path = expname[[l]], title = readTitle(path)[[3]])
      }
    }

    res$title <- data.table(do.call("rbind", lst))

    if (nrow(res$title) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found titles\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$title),
                               "found titles\n")))
    }
  }

  if ("eretic" %in% what | "all" %in% what) {
    lst <- list()

    for (l in 1:length(expname)) {

      ereticPath <- file.path(expname[[l]])

      if (file.exists(file.path(ereticPath, "QuantFactorSample.xml"))) {
        # we look for eretic in the rexpno + 0 folder
        # to correct for pgpe and other type of experiments that need correction
        eretic <- readEretic(file.path(ereticPath, "QuantFactorSample.xml"))
        ereticFactor <- eretic$ereticFactor

      } else if (file.exists(file.path(ereticPath, "pdata", "1", "eretic_file.xml"))) {
        # in the case of F80 we use a different parser
        eretic <- readEreticF80(file.path(ereticPath, "pdata", "1", "eretic_file.xml"))
        ereticFactor <- eretic$samOneMolInt

      } else {
        ereticFactor <- NULL

      }


      if (!is.null(ereticFactor)) {
        lst[[l]] <- c(path = expname[[l]], ereticFactor = ereticFactor)
      }


    }


    res$eretic <- data.table(do.call("rbind", lst))
    if (nrow(res$eretic) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found ereticFactors\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$eretic),
                               "found ereticFactors\n")))
    }


  }

  ifelse("procno" %in% names(opts), procno <- opts$procno, procno <- 1)

  if ("spec" %in% what | "all" %in% what | "specOnly" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {

      if("specOpts" %in% names(opts)) {
        specOpts <- opts$specOpts
      } else {
        specOpts = list(uncalibrate = FALSE,
                        fromTo = c(-0.1, 10),
                        length.out = 44079)
      }

      # we always look in the first folder (ANPC folder structure)
      # this is pretty safe but may be wrong for some reruns
      ereticPath <- paste0(substr(expname[[l]], 1, nchar(expname[[l]])-1), "0")

      if ("eretic" %in% names(specOpts)) {

        # we first look for eretic in the options (so we can override it if necessary)
        # in cases such as ileum spiking that has a wrong folder structure
        ereticFactor <- specOpts$eretic
      } else if (file.exists(file.path(ereticPath, "QuantFactorSample.xml"))) {
        # we look for eretic in the rexpno + 0 folder
        # to correct for pgpe and other type of experiments that need correction
        eretic <- readEretic(file.path(ereticPath, "QuantFactorSample.xml"))
        ereticFactor <- eretic$ereticFactor

      } else if (file.exists(file.path(ereticPath, "pdata", "1", "eretic_file.xml"))) {
        # in the case of F80 we use a different parser
        eretic <- readEreticF80(file.path(ereticPath, "pdata", "1", "eretic_file.xml"))
        ereticFactor <- eretic$samOneMolInt

      } else {
        # if nothing was found we set the factor to 1 and display a warning
        ereticFactor <- 1
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
    if (nrow(res$spec) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found spectrum(a)\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$spec),
                               "found spectrum(a)\n")))
    }
  }

  if ("lipo" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {

      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*lipo.*\\.xml$")
      if(any(grepl("1_1_0", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0", path)]
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
      message(cat(crayon::yellow("readExperiment >> 0 found lipo\n")))
    } else {
      lipo <- lapply(res$lipo$data,
                     function(x) reshape(setDT(x)[, .(id, path, value, unit, refMax, refMin),],
                                         idvar = "path",
                                         timevar = "id",
                                         direction = "wide"))
      lipo <- do.call("rbind", lipo)
      res$lipo <- lipo
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$lipo),
                               "found lipo\n")))
    }
  }

  if ("pacs" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {
      folderPath <- file.path(expname[[l]], "pdata", "1")
      path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*pacs.*\\.xml$")
      if(any(grepl("1_1_0", path))) {
        # if more than 1 version available pick the latest ones
        path <- path[grepl("1_1_0", path)]
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
      message(cat(crayon::yellow("readExperiment >> 0 found pacs\n")))
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
      message(cat(crayon::blue("readExperiment >>", nrow(res$pacs), "found pacs\n")))
    }
  }

  if ("quant" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(expname)) {
      folderPath <- file.path(expname[[l]], "pdata", "1")
      # path = dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*quant.*\\.xml$")
      # if(any(grepl("2_1_0", path))) {
      #   # if more than 1 version available pick the latest ones  "SERUM"
      #   path <- path[grepl("2_1_0", path)]
      # } else if (any(grepl("1_2_0", path))) {
      #   # if more than 1 version available pick the latest ones."Urine"
      #   path <- path[grepl("1_2_0", path)]
      # } else if (any(grepl("1_2_0", path))) {
      #   # if more than 1 version available pick the latest ones."Urine"
      #   path <- path[grepl("1_2_0", path)]
      # }
      # if (length(path)==1) {
      #   quant <- readQuant(path)
      # }  else {
      #   quant <- NULL
      # }
      # Define version priority (highest first)
      priority <- c("plasma_quant_report_2_1_0.xml", "urine_quant_report_e_1_2_0.xml",  "plasma_quant_report.xml",
                    "urine_quant_report_e.xml","urine_quant_report_e_ver_1_0.xml",
                    "urine_quant_report_b.xml","urine_quant_report_b_ver_1_0.xml",
                    "urine_quant_report_ne.xml","urine_quant_report_ne_ver_1_0.xml")
      
      # Find all matching xml files
      path <- dir(folderPath, full.names = TRUE, recursive = TRUE, pattern = ".*quant.*\\.xml$")
      
      # Pick the highest-priority match
      chosen <- NULL
      for (ver in priority) {
        matches <- grep(ver, path, value = TRUE)
        if (length(matches) > 0) {
          chosen <- matches
          break
        }
      }
      
      # If exactly one file matched, read it
      if (!is.null(chosen) && length(chosen) == 1) {
        quant <- readQuant(chosen)
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
      message(cat(crayon::yellow("readExperiment >> 0 found quant\n")))
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
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$quant),
                               "found quant\n")))
    }
  }

  return(res)
}


# path <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/11"
# exp <- readExperiment(path)
