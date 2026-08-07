#' extract pacs information from a bruker xml
#'
#' @param file - the path to the expName folder
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_attr

readPacs <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")

    # each PARAMETER's VALUE and REFERENCE are read from inside that
    # PARAMETER, so that one of them missing gives NA in place rather than
    # shifting every later parameter onto its neighbour's numbers
    params <- xml_find_all(xml, ".//PARAMETER")
    values <- lapply(params, function(p) xml_find_first(p, "./VALUE"))
    refs <- lapply(params, function(p) xml_find_first(p, "./REFERENCE"))

    name <- xml_attr(params, "name")
    conc_v <- vapply(values, xml_attr, character(1), "conc")
    concUnit_v <- vapply(values, xml_attr, character(1), "concUnit")
    refMax <- vapply(refs, xml_attr, character(1), "vmax")
    refMin <- vapply(refs, xml_attr, character(1), "vmin")
    refUnit <- vapply(refs, xml_attr, character(1), "unit")
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
    return(NULL)
  }
}

