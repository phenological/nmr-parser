#' read experiment from a bruker folder (expno)
#'
#' @param file - a path (or list of) to the expNo folder(s)
#' @param what - choose what to read (acqus, procs, qc, title, eretic, spec, lipo, quant, pacs, all)
#' @param options - object with processing parameters, if TRUE reads from the file
#' @return a list with all read elements
#'
#' @export
#' @importFrom stats reshape
#' @importFrom data.table setDT
# curl -X 'GET' $ROLDX_URL/link\?runName\=EXTr01 | jq '.list | .noesygppr1d | .[0] | keys'
readExperiment <- function(file,
                           options = list(what = c("acqus",
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
                                          specOpts = list(uncalibrate = FALSE,
                                                          fromTo = c(-0.1, 10),
                                                          length.out = 44079))) {

  name <- path <- conc_v <- concUnit_v <- refMax <- refMin <- NULL
  id <- value <- unit <- rawConc <- NULL

  what <- options$what

  if (is.character(file)) {
    file <- as.list(file)
  }

  res <- list()

  if ("acqus" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {
      cat("Reading:", l, "/", length(file), "\r")
      
      path <- file.path(file[[l]], "acqus")
      if (file.exists(path)) {
        parms <- readParams(path)
        parms$path <- file[[l]]
        lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
      }
    }
    common_columns <- Reduce(intersect, lapply(lst, names))
    res$acqus <- data.table(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
    colnames(res$acqus) <- gsub("value.", "acqus.", colnames(res$acqus))
    message(cat(crayon::blue("readExperiment >>",
                             nrow(res$acqus),
                             "found acqus params\n")))
  }

  if ("procs" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {
      cat("Reading:", l, "/", length(file), "\r")
      
      path <- file.path(file[[l]], "pdata", "1", "procs")
      if (file.exists(path)) {
        parms <- readParams(path)
        parms$path <- file[[l]]
        lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
      }
    }
    common_columns <- Reduce(intersect, lapply(lst, names))
    res$procs <- data.table(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
    colnames(res$procs) <- gsub("value.", "procs.", colnames(res$procs))
    message(cat(crayon::blue("readExperiment >>",
                             nrow(res$procs),
                             "found procs params\n")))
  }

  if ("qc" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {
      cat("Reading:", l, "/", length(file), "\r")
      
      path_serum <- file.path(file[[l]], "pdata", "1", "plasma_qc_report.xml")
      path_urine <- file.path(file[[l]], "pdata", "1", "urine_qc_report.xml")
      if (file.exists(path_serum)) {
        qc <- readQc(path_serum)
      } else if (file.exists(path_urine)) {
        qc <- readQc(path_urine)
      } else {
        qc <- NULL
      }
      if (!is.null(qc)) {
        qc$path <- file[[l]]
        lst[[l]] <- qc
      }
    }

    res$qc <- data.table(do.call("rbind", lst))
    if (is.null(nrow(res$qc))) {
      message(cat(crayon::yellow("readExperiment >> 0 found qc\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>", nrow(res$qc), "found qc\n")))
    }
  }

  if ("title" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {

      path <- file.path(file[[l]], "pdata", "1", "title")
      if (file.exists(path)) {
        lst[[l]] <- c(path = file[[l]], title = readTitle(path)[[3]])
      }
    }

    res$title <- data.table(do.call("rbind", lst))
    message(cat(crayon::blue("readExperiment >>",
                             nrow(res$title),
                             "found titles\n")))
  }

  if ("eretic" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {

      if (file.exists(file.path(file[[l]], "QuantFactorSample.xml"))) {
        eretic <- readEretic(file.path(file[[l]], "QuantFactorSample.xml"))
        ereticFactor <- eretic$ereticFactor
      } else if (file.exists(file.path(file[[l]], "pdata", "1", "eretic_file.xml"))) {
        eretic <- readEreticF80(file.path(file[[l]], "pdata", "1", "eretic_file.xml"))
        ereticFactor <- eretic$samOneMolInt
      } else {
        ereticFactor <- NULL
      }
      if (!is.null(ereticFactor)) {
        lst[[l]] <- c(path = file[[l]], ereticFactor = ereticFactor)
      }
    }

    res$eretic <- data.table(do.call("rbind", lst))
    message(cat(crayon::blue("readExperiment >>",
                             nrow(res$eretic),
                             "found ereticFactors\n")))
  }

  if ("spec" %in% what | "all" %in% what | "specOnly" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {
      if("specOpts" %in% names(options)) {
        specOpts <- options$specOpts
      } else {
        specOpts = list(uncalibrate = FALSE,
                        fromTo = c(-0.1, 10),
                        length.out = 44079)
      }

      if (file.exists(file.path(file[[l]], "QuantFactorSample.xml"))) {
        eretic <- readEretic(file.path(file[[l]], "QuantFactorSample.xml"))
        ereticFactor <- eretic$ereticFactor
      } else if (file.exists(file.path(file[[l]], "pdata", "1", "eretic_file.xml"))) {
        eretic <- readEreticF80(file.path(file[[l]], "pdata", "1", "eretic_file.xml"))
        ereticFactor <- eretic$samOneMolInt
      } else {
        ereticFactor <- NULL
      }

      if (!is.null(ereticFactor)) {
        specOpts$eretic <- ereticFactor
      }
      spec <- readSpectrum(file[[l]], procs = TRUE, options = specOpts)
      if (!is.null(spec)) {
        lst[[l]] <- list(path = file[[l]], spec = spec)
      }
    }

    res$spec <- data.table(do.call("rbind", lst))
    if (length(spec) == 0) {
      message(cat(crayon::yellow("readExperiment >> 0 found spectrum(a)\n")))
    } else {
      message(cat(crayon::blue("readExperiment >>",
                               nrow(res$spec),
                               "found spectrum(a)\n")))
    }
  }

  if ("lipo" %in% what | "all" %in% what) {
    lst <- list()
    for (l in 1:length(file)) {

      path <- file.path(file[[l]], "pdata", "1", "lipo_results.xml")
      if (file.exists(path)) {
        lipoproteins <- readLipo(path)
        if (!is.null(lipoproteins)) {
          lipoproteins$data$path <- file[[l]]
          lst[[l]] <- lipoproteins
        }
      }
    }

    res$lipo <- data.table(do.call("rbind", lst))
    if (length(res$lipo) == 0) {
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
    for (l in 1:length(file)) {

      path <- file.path(file[[l]], "pdata", "1", "plasma_pacs_report.xml")
      if (file.exists(path)) {
        pacs <- readPacs(path)
      } else {
        pacs <- NULL
      }
      if (!is.null(pacs)) {
        pacs$data$path <- file[[l]]
        lst[[l]] <- pacs
      }
    }

    res$pacs <- data.table(do.call("rbind", lst))

    if (length(res$pacs) == 0) {
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
    for (l in 1:length(file)) {
      path_serum <- file.path(file[[l]], "pdata", "1", "plasma_quant_report.xml")
      path_urine <- file.path(file[[l]], "pdata", "1", "urine_quant_report_e.xml")
      if (file.exists(path_serum)) {
        quant <- readQuant(path_serum)
      } else if (file.exists(path_urine)) {
        quant <- readQuant(path_urine)
      } else {
        quant <- NULL
      }
      if (!is.null(quant)) {
        quant$data$path <- file[[l]]
        lst[[l]] <- quant
      }
    }

    res$quant <- data.table(do.call("rbind", lst))
    if (length(res$quant) == 0) {
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
