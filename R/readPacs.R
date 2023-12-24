#' extract pacs information from a bruker xml
#'
#' @param file - the path to the expName folder
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attr xml_attrs

readPacs <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")
    name <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    conc_v <- xml_attr(xml_find_all(xml, ".//PARAMETER/VALUE"), "conc")
    concUnit_v <- xml_attr(xml_find_all(xml, ".//PARAMETER/VALUE"), "concUnit")
    refMax <- xml_attr(xml_find_all(xml, ".//PARAMETER/REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//PARAMETER/REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//PARAMETER/REFERENCE"), "unit")
    version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")
    data <- data.frame(name,
                       conc_v,
                       concUnit_v,
                       refMax,
                       refMin,
                       refUnit)
    return(list(data = data, version = version))
  } else {
    cat(crayon::yellow("readPacs >>", file, "not found\n"))
  }
}

