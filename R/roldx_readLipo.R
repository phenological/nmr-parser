#' get lipo for rolodex
#' @param file - path to the data
#' @return a triad
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_find_first
#' @export
roldx_readLipo <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")
    version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")
    version <- strsplit(version, " ")[[1]][1]
    id <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "value")

    # there is one duplicated id
    fi <- duplicated(id)
    return(list(id = id[!fi], value = as.numeric(value[!fi]), version = version))
  } else {
    cat(crayon::yellow("fusion::readLipo >>", file, "not found\n"))
  }
}
