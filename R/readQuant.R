#' extract small molecules  quantification information from a bruker xml
#'
#' @param file - the path to the expName folder
#' @return the values
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_attr
#' @export

readQuant <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")
    version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")

    # Ask the document which shape it is, not the file name. Bruker gives
    # both shapes the same version attribute (plasma_quant_report.xml and
    # plasma_quant_report_ver_1_0.xml both say "Quant-PS 2.0.0"), so the
    # version cannot tell them apart and the name used to. A name is an
    # external label: copy a report out of pdata/1 under a tidier name and
    # the wrong branch reads it, which yields a full set of compounds with
    # every concentration NA rather than an error.
    # The two shapes are unambiguous in their content: this one puts
    # valueext on its VALUE nodes and carries no RELDATA.
    isExtended <- length(xml_find_all(xml, ".//VALUE[@valueext]")) > 0
    hasRelData <- length(xml_find_all(xml, ".//RELDATA")) > 0

    if (isExtended) {
      param <- xml_find_all(xml, ".//PARAMETER")
      paramList <- list()
      for (p in param) {
        name <- xml_attr(p, "name")

        # the absolute concentration is on the first VALUE and the
        # creatinine relative one on the second, where there is one
        values <- xml_find_all(p, "./VALUE")
        firstValue <- xml_find_first(p, "./VALUE")
        reference <- xml_find_first(p, "./REFERENCE")
        relative <- function(attribute) xml_attr(values, attribute)[2]

        conc_v <- xml_attr(firstValue, "valueext")
        concUnit_v <- xml_attr(firstValue, "unit")
        lod_v <- xml_attr(firstValue, "lod")
        lodUnit_v <- xml_attr(firstValue, "unit")
        loq_v <- xml_attr(firstValue, "loq")
        loqUnit_v <- xml_attr(firstValue, "unit")
        conc_vr <- relative("valueext")
        concUnit_vr <- relative("unit")
        lod_vr <- relative("lod")
        lodUnit_vr <- relative("unit")
        loq_vr <- relative("loq")
        loqUnit_vr <- relative("unit")
        sigCorrUnit <- NA
        sigCorr <- NA
        rawConcUnit <- xml_attr(firstValue, "unit")
        rawConc <- xml_attr(firstValue, "valueext")
        errConc <- NA
        errConcUnit <- NA
        # creatinine is the reference itself, its own range is not reported
        if (name != "Creatinine") {
          # xml_find_first, not xml_find_all: a parameter carrying no
          # REFERENCE gave a zero row data.frame and dropped out of the
          # report entirely, one carrying two gave two rows
          refMax <- xml_attr(reference, "vmax")
          refMin <- xml_attr(reference, "vmin")
          refUnit <- xml_attr(reference, "unit")

        } else {
          refMax <- NA
          refMin <- NA
          refUnit <- NA

        }

        # appended, not keyed by name: a repeated compound used to
        # overwrite the earlier one instead of getting its own row
        paramList[[length(paramList) + 1]] <- data.frame(name,
                                        conc_v,
                                        concUnit_v,
                                        lod_v,
                                        lodUnit_v,
                                        loq_v,
                                        loqUnit_v,
                                        conc_vr,
                                        concUnit_vr,
                                        lod_vr,
                                        lodUnit_vr,
                                        loq_vr,
                                        loqUnit_vr,
                                        sigCorrUnit,
                                        sigCorr,
                                        rawConcUnit,
                                        rawConc,
                                        errConc,
                                        errConcUnit,
                                        refMax, refMin, refUnit)
      }
      data <- data.frame(do.call(rbind, paramList))

    } else if (hasRelData && grepl("Quant", version, fixed = TRUE)) {

      # each PARAMETER carries its own VALUE, VALUERELATIVE, RELDATA and
      # REFERENCE, so read them from inside it. Sweeping the whole document
      # once per attribute lines the lists up by position, and the lists are
      # not the same length: Creatinine has no VALUERELATIVE, which is what
      # the c(NA, ...) that used to sit on the six _vr vectors was padding
      # for. That pad is only right while the parameter without a
      # VALUERELATIVE is the first one
      params <- xml_find_all(xml, ".//PARAMETER")
      values <- lapply(params, function(p) xml_find_first(p, "./VALUE"))
      valuesRel <- lapply(params, function(p) xml_find_first(p, "./VALUERELATIVE"))
      relData <- lapply(params, function(p) xml_find_first(p, "./RELDATA"))
      refs <- lapply(params, function(p) xml_find_first(p, "./REFERENCE"))

      name <- xml_attr(params, "name")
      conc_v <- vapply(values, xml_attr, character(1), "conc")
      concUnit_v <- vapply(values, xml_attr, character(1), "concUnit")
      lod_v <- vapply(values, xml_attr, character(1), "lod")
      lodUnit_v <- vapply(values, xml_attr, character(1), "lodUnit")
      loq_v <- vapply(values, xml_attr, character(1), "loq")
      loqUnit_v <- vapply(values, xml_attr, character(1), "loqUnit")
      conc_vr <- vapply(valuesRel, xml_attr, character(1), "conc")
      concUnit_vr <- vapply(valuesRel, xml_attr, character(1), "concUnit")
      lod_vr <- vapply(valuesRel, xml_attr, character(1), "lod")
      lodUnit_vr <- vapply(valuesRel, xml_attr, character(1), "lodUnit")
      loq_vr <- vapply(valuesRel, xml_attr, character(1), "loq")
      loqUnit_vr <- vapply(valuesRel, xml_attr, character(1), "loqUnit")
      sigCorrUnit <- vapply(relData, xml_attr, character(1), "sigCorrUnit")
      sigCorr <- vapply(relData, xml_attr, character(1), "sigCorr")
      rawConcUnit <- vapply(relData, xml_attr, character(1), "rawConcUnit")
      rawConc <- vapply(relData, xml_attr, character(1), "rawConc")
      errConc <- vapply(relData, xml_attr, character(1), "errConc")
      errConcUnit <- vapply(relData, xml_attr, character(1), "errConcUnit")
      refMax <- vapply(refs, xml_attr, character(1), "vmax")
      refMin <- vapply(refs, xml_attr, character(1), "vmin")
      refUnit <- vapply(refs, xml_attr, character(1), "unit")
      version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")

      data <- data.frame(name,
                         conc_v,
                         concUnit_v,
                         lod_v,
                         lodUnit_v,
                         loq_v,
                         loqUnit_v,
                         conc_vr,
                         concUnit_vr,
                         lod_vr,
                         lodUnit_vr,
                         loq_vr,
                         loqUnit_vr,
                         sigCorrUnit,
                         sigCorr,
                         rawConcUnit,
                         rawConc,
                         errConc,
                         errConcUnit,
                         refMax, refMin, refUnit)
    } else {
      # neither shape, rather than the wrong one read silently
      cat(crayon::red("readQuant >>", file, "version not recognized:", version, "\n"))
      return(NULL)
    }

    return(list(data = data, version = version))

  } else {

    cat(crayon::yellow("readQuant >>", file, "not found\n"))
    return(NULL)

  }
}

