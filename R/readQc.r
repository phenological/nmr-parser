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

    infos <- list(name = (xml_attr(xml_find_all(xml, ".//INFO"), "name")),
               value = (xml_attr(xml_find_all(xml, ".//INFO"), "value")))
    # infos <- mapply(list, infos$name, infos$value, SIMPLIFY = FALSE)
    # infos <- lapply(infos, function(x) setNames(x, c("name", "value")))
    infoNames <- unname(sapply(infos$name, function(x) cleanNames(strsplit(tolower(x), "\\(")[[1]][1])))

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
    # tests <- mapply(list,
    #                 tests$comment,
    #                 tests$name,
    #                 tests$type,
    #                 tests$value,
    #                 tests$unit,
    #                 tests$refMax,
    #                 tests$refMin,
    #                 SIMPLIFY = FALSE)
    # infos <- lapply(infos, function(x) setNames(x, c("comment",
    #                                                  "name",
    #                                                  "type",
    #                                                  "value",
    #                                                  "unit",
    #                                                  "source",
    #                                                  "unit",
    #                                                  "vmax",
    #                                                  "vmin")))

    res <- list(infos = infos,
                infoNames = infoNames,
                tests = tests,
                testNames = testNames)

    return(res)
  } else {
    cat(crayon::yellow("readQc >>", file, "not found\n"))
  }

}



