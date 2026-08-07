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
    # check that "name" exists in xml
    if (grepl("_ver_",file,fixed = TRUE) ) {
      param <- xml_find_all(xml, ".//PARAMETER")
      paramList <- list()
      for (p in param) {
        name <- xml_attr(p, "name")
        conc_v <- xml_attr(xml_find_first(p, ".//VALUE"), "valueext")
        concUnit_v <- xml_attr(xml_find_first(p, ".//VALUE"), "unit")
        lod_v <- xml_attr(xml_find_first(p, ".//VALUE"), "lod")
        lodUnit_v <- xml_attr(xml_find_first(p, ".//VALUE"), "unit")
        loq_v <- xml_attr(xml_find_first(p, ".//VALUE"), "loq")
        loqUnit_v <- xml_attr(xml_find_first(p, ".//VALUE"), "unit")
        conc_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "valueext")[2]
        concUnit_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "unit")[2]
        lod_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "lod")[2]
        lodUnit_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "unit")[2]
        loq_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "loq")[2]
        loqUnit_vr <- xml_attr(xml_find_all(p, ".//VALUE"), "unit")[2]
        sigCorrUnit <- NA
        sigCorr <- NA
        rawConcUnit <- xml_attr(xml_find_first(p, ".//VALUE"), "unit")
        rawConc <- xml_attr(xml_find_first(p, ".//VALUE"), "valueext")
        errConc <- NA
        errConcUnit <- NA
        if (name != "Creatinine") {
          refMax <- xml_attr(xml_find_all(p, ".//REFERENCE"), "vmax")
          refMin <- xml_attr(xml_find_all(p, ".//REFERENCE"), "vmin")
          refUnit <- xml_attr(xml_find_all(p, ".//REFERENCE"), "unit")

        } else {
          refMax <- NA
          refMin <- NA
          refUnit <- NA

        }
        version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")


        paramList[[name]] <- data.frame(name,
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

    }else if (grepl("Quant", version, fixed = TRUE) ){

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
      cat(crayon::red("readQuant >>", file, "version not recognized\n"))
      return(NULL)
    }

    return(list(data = data, version = version))

  } else {

    cat(crayon::yellow("readQuant >>", file, "not found\n"))
    return(NULL)

  }
}

