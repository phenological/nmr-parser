#' read Qc info for importation into rolodex
#' extract qc information
#' @param file - the path to the expName folder (plamsa_qc_report.xml / urine_qc_report.xml)
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_attrs xml_find_all
#' @importFrom stats setNames
readQc <- function(file){
  if (file.exists(file)) {
    xml <- read_xml(file, options = "NOBLANKS")

    # {xml_document}
    # <RESULTS version="BioBankQC PS 1.0.0  Reseach Use Only">
    # [1] <SAMPLE date="23-Mar-2021 20:22:12" name="bhas20_IVDR04_BHASp27_220321_expno840.1000 ...
    version <- xml_attr(xml, "version")

    name <- xml_attr(xml_find_all(xml, ".//INFO"), "name")

    value <- unname(sapply(name, function(x) gsub("\\)", "",(strsplit(tolower(x), "applied: ")[[1]][2]))))
    ref <- unname(sapply(name, function(x) strsplit((strsplit(tolower(x), "specified: ")[[1]][2]), ",")[[1]][1]))
    comment <- (xml_attr(xml_find_all(xml, ".//INFO"), "value"))

    name <- gsub(" $", "", (unname(sapply(name, function(x) strsplit(x, "\\(")[[1]][1]))))
    infoNames <- cleanNames(name)
    infos <- list(name = name,
                  comment = comment,
                  value = value)


    tests <- xml_find_all(xml, ".//PARAMETER")
    names <- xml_attr(tests, "name")
    testNames <- unname(sapply(names, function(x) cleanNames(strsplit(tolower(x), "\\(")[[1]][1])))
    headers <- list(comment = unlist(xml_attr(xml_find_all(tests, "//PARAMETER"), "comment")),
                    name = unlist(xml_attr(xml_find_all(tests, "//PARAMETER"), "name")),
                    type = unlist(xml_attr(xml_find_all(tests, "//PARAMETER"), "type")))
    vals <- lapply(tests, function(tests) {
      cnt <- c(value = unlist(xml_attr(xml_find_first(tests, ".//VALUE"), "value")),
                  unit = unlist(xml_attr(xml_find_first(tests, ".//VALUE"), "unit")),
                  refMax = unlist(xml_attr(xml_find_first(tests, ".//REFERENCE"), "vmax")),
                  refMin = unlist(xml_attr(xml_find_first(tests, ".//REFERENCE"), "vmin")))

      # some values need cleaning
      cnt <- gsub("\\\\textless", "< ", cnt)

      return(cnt)
    })

    vals <- data.table(do.call(rbind, vals))
    vals <- lapply(vals, function(column) setNames(column, vals$Name))
    tests <- c(headers, vals)


    res <- list(data = list(infos = infos,
                infoNames = infoNames,
                tests = tests,
                testNames = testNames),
                version = version)

    return(res)
  } else {
    cat(crayon::yellow("readQc >>", file, "not found\n"))
  }

}




