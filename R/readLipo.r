#' extract lipoprotein  quantification information from a bruker xml
#'
#' @param file - the path to the expName folder
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_find_first
readLipo <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")
    comment <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "comment")
    id <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    type <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "type")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "value")
    unit <- xml_attr(xml_find_all(xml, ".//VALUE"), "unit")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")
    version <- xml_attr(xml_find_first(xml, ".//QUANTIFICATION"), "version")
    version <- strsplit(version, " ")[[1]][1]

    fraction <- sapply(comment, function(x) strsplit(x, ",")[[1]][1])
    name <- sapply(comment, function(x) strsplit(x, ",")[[1]][2])
    abbr <- sapply(comment, function(x) strsplit(x, ",")[[1]][3])

    res <- data.frame(fraction,
                      name,
                      abbr,
                      id,
                      type,
                      value = as.numeric(value),
                      unit,
                      refMax = as.numeric(refMax),
                      refMin = as.numeric(refMin),
                      refUnit)
    fi <- duplicated(res$id)
    return(list(data = res[!fi,], version = version))
  } else {
    cat(crayon::yellow("readLipo >>", file, "not found\n"))
  }
}

# addValues(lip, options = list(fold = TRUE, scale = FALSE, dotColor = "red"))
# makeLipoReport(lip, options = list(fold = FALSE, dotColor = "blue", dotPch = 21))
# addValues(lip, options = list(dotColor = "red", dotPch = 22))



