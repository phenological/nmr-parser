#' read Qc info for importation into rolodex
#'
#' @param file - the path to the expName folder (plamsa_qc_report.xml / urine_qc_report.xml)
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_attrs xml_find_all
#' @importFrom stats setNames
readQc <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")

    infos <- list(name = (xml_attr(xml_find_all(xml, ".//INFO"), "name")),
               value = (xml_attr(xml_find_all(xml, ".//INFO"), "value")))
    infos <- mapply(list, infos$name, infos$value, SIMPLIFY = FALSE)
    infos <- lapply(infos, function(x) setNames(x, c("name", "value")))
    infoNames <- unname(sapply(infos, function(x) cleanNames(strsplit(tolower(x), "\\(")[[1]][1])))

    tests <- xml_find_all(xml, ".//PARAMETER")
    names <- xml_attr(tests, "name")
    testNames <- unname(sapply(names, function(x) cleanNames(strsplit(tolower(x), "\\(")[[1]][1])))
    tests <- lapply(tests, function(x) {
      c(unlist(xml_attrs(x, ".//PARAMETER")),
        value = unlist(xml_attr(xml_find_all(x, ".//VALUE"), "value")),
        unit = unlist(xml_attr(xml_find_all(x, ".//VALUE"), "unit")),
        xml_attrs(xml_find_all(x, ".//REFERENCE")))
    })

    res <- list(infos = infos,
                infoNames = infoNames,
                tests = tests,
                testNames = testNames)

    return(res)
  } else {
    cat(crayon::yellow("fusion::readQc >>", file, "not found\n"))
  }

}



