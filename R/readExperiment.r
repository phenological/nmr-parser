#' read experiment from a bruker folder (expno)
#'
#' @param path - the path to the expNo folder
#' @param acqus - object with acquisiton parameters, if TRUE reads from the file
#' @param procs - object with processing parameters, if TRUE reads from the file
#' @return a list with all read elements
#'
#' @export
#' @importFrom stats reshape
readExperiment <- function(path, what = c("acqus",
                                          "procs",
                                          "title",
                                          "eretic",
                                          "spec",
                                          "lipo",
                                          "ivdr",
                                          "pacs",
                                          "all",
                                          "specOnly"),
                           options = list(specOpts = list(uncalibrate = FALSE,
                                                          fromTo = c(-0.1, 10),
                                                          length.out = 44079))) {

  if (is.character(path)) {
    path <- as.list(path)
  }

  res <- list()

  lst <- list()
  for (l in 1:length(path)) {
    if ("acqus" %in% what | "all" %in% what) {
      parms<- readParams(file.path(path[[l]], "acqus"))
      lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
    }
  }
  common_columns <- Reduce(intersect, lapply(lst, names))
  res$acqus <- data.frame(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
  cat(crayon::blue("readExperiment >>", nrow(res$acqus), "found acqus params\n"))

  lst <- list()
  for (l in 1:length(path)) {
    if ("procs" %in% what | "all" %in% what) {
      parms <- readParams(file.path(path[[l]], "pdata", "1", "procs"))
      lst[[l]] <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
    }
  }
  common_columns <- Reduce(intersect, lapply(lst, names))
  res$procs <- data.frame(do.call("rbind", lapply(lst, function(vec) vec[common_columns])))
  cat(crayon::blue("readExperiment >>", nrow(res$procs), "found acqus params\n"))

  lst <- list()
  for (l in 1:length(path)) {
    if ("title" %in% what | "all" %in% what) {
      lst[[l]] <- list(path = path[[l]], title = readTitle(file.path(path[[l]], "pdata", "1", "title"))[[3]])
    }
  }
  res$title <- do.call("rbind", lst)
  cat(crayon::blue("readExperiment >>", nrow(res$title), "found titles\n"))

  lst <- list()
  for (l in 1:length(path)) {
    if ("eretic" %in% what | "all" %in% what |
        "specOnly" %in% what | "spec" %in% what) {
      if (file.exists(file.path(path[[l]], "QuantFactorSample.xml"))) {
        eretic <- readEretic(path[[l]])
        ereticFactor <- eretic$ereticFactor
      } else if (file.exists(file.path(path[[l]], "pdata", "1", "eretic_file.xml"))) {
        eretic <- readEreticF80(path[[l]])
        ereticFactor <- eretic$samOneMolInt
      } else {
        ereticFactor <- NULL
      }
      lst[[l]] <- ereticFactor
    }
  }
  res$eretic <- do.call("rbind", lst)
  cat(crayon::blue("readExperiment >>", nrow(res$eretic), "found ereticFactors\n"))

  lst <- list()
  for (l in 1:length(path)) {
    if ("spec" %in% what | "all" %in% what | "specOnly" %in% what) {
      specOpts <- options$specOpts

      if (!is.null(eretic)) {
        specOpts$eretic <- ereticFactor
      }

      res$spec <- readSpectrum(path, procs = TRUE, options = specOpts)
    }
  }
  res$spec <- do.call("rbind", lst)
  cat(crayon::blue("readExperiment >>", nrow(res$spec), "found spectrum(a)\n"))

  if ("lipo" %in% what | "all" %in% what) {
    if (file.exists(file.path(path, "pdata", "1", "lipo_results.xml"))) {
      lipoproteins <- readLipo(path)
      if (!is.null(lipoproteins)) {
        res$lipo <- lipoproteins
      }
    }
  }


  # if ("pacs" %in% what | "all" %in% what) {
  #   if (file.exists(file.path(path, "pdata", "1", "plasma_pacs_report.xml"))) {
  #     pacs <- readPacs(path)
  #     if (!is.null(pacs)) {
  #       res$pacs <- pacs
  #     }
  #   }
  # }

  if ("ivdr" %in% what | "all" %in% what) {
    if (file.exists(file.path(path, "pdata", "1", "plasma_quant_report.xml"))) {
      ivdr <- readQuant(path, "plasma_quant_report.xml")
    } else if (file.exists(file.path(path, "pdata", "1", "urine_quant_report_e.xml"))) {
      ivdr <- readQuant(path, "urine_quant_report_e.xml")
    } else {
      ivdr <- NULL
    }
    if (!is.null(ivdr)) {
      res$ivdr <- ivdr
    }
  }

  return(res)
}


# path <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/11"
# exp <- readExperiment(path)
