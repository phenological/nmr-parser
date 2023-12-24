#' extract small molecules  quantification information from a bruker xml
#'
#' @param file - the path to the expName folder
#' @return the values
#' @importFrom xml2 xml_find_first xml_find_all xml_attr
#' @export

readQuant <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")
    name <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    conc_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "conc")
    concUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "concUnit")
    lod_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "lod")
    lodUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "lodUnit")
    loq_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "loq")
    loqUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "loqUnit")
    conc_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "conc"))
    concUnit_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "concUnit"))
    lod_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "lod"))
    lodUnit_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "lodUnit"))
    loq_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "loq"))
    loqUnit_vr <- c(NA, xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "loqUnit"))
    sigCorrUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "sigCorrUnit")
    sigCorr <- xml_attr(xml_find_all(xml, ".//RELDATA"), "sigCorr")
    rawConcUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "rawConcUnit")
    rawConc <- xml_attr(xml_find_all(xml, ".//RELDATA"), "rawConc")
    errConc <- xml_attr(xml_find_all(xml, ".//RELDATA"), "errConc")
    errConcUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "errConcUnit")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")
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
    return(list(data = data, version = version))
  } else {
    cat(crayon::yellow("readQuant >>", file, "not found\n"))
  }
}

