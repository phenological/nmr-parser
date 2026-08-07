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

    # take each PARAMETER's VALUE and REFERENCE from inside that PARAMETER.
    # Sweeping the whole document once per attribute instead lines the lists
    # up by position, which silently shifts every later parameter onto its
    # neighbour's numbers as soon as one of them carries a different number
    # of children
    params <- xml_find_all(xml, ".//PARAMETER")
    values <- lapply(params, function(p) xml_find_first(p, ".//VALUE"))
    refs <- lapply(params, function(p) xml_find_first(p, ".//REFERENCE"))

    comment <- xml_attr(params, "comment")
    id <- xml_attr(params, "name")
    type <- xml_attr(params, "type")
    value <- vapply(values, xml_attr, character(1), "value")
    unit <- vapply(values, xml_attr, character(1), "unit")
    refMax <- vapply(refs, xml_attr, character(1), "vmax")
    refMin <- vapply(refs, xml_attr, character(1), "vmin")
    refUnit <- vapply(refs, xml_attr, character(1), "unit")
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
    # these reports carry LDCH and HDCH twice, keep the first of each
    fi <- duplicated(res$id)

    return(list(data = res[!fi,], version = version))

  } else {
    cat(crayon::yellow("readLipo >>", file, "not found\n"))
  }
}

# addValues(lip, options = list(fold = TRUE, scale = FALSE, dotColor = "red"))
# makeLipoReport(lip, options = list(fold = FALSE, dotColor = "blue", dotPch = 21))
# addValues(lip, options = list(dotColor = "red", dotPch = 22))



