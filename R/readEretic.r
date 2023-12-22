#' extract xml quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the title
#'
#' @export
#' @importFrom xml2 read_xml xml_text xml_find_first xml_attr xml_attrs
readEretic <- function(path){
  path <- file.path(path)
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    calEreticPosition <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Position")))
    calEreticLineWidth <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Line_Width")))
    calEreticConcentration <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Concentration")))
    calTubeID <- xml_attr(xml_find_all(xml, ".//Eretic_Sample_Tube"), "ID")
    calTmin <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_min")))
    calTmax <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_max")))
    calP1 <- as.numeric(xml_text(xml_find_all(xml, ".//Eretic_Calibration//P1")))
    calEreticCalibration <- as.numeric(xml_text(xml_find_all(xml, ".//Eretic_Calibration//Eretic_Factor")))
    ereticFactor <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//Eretic_Factor")))
    P1 <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//P1")))
    temperature <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//Temperature")))
    df <- data.frame(field = 600,
                     calEreticPosition,
                     calEreticLineWidth,
                     calEreticConcentration,
                     calTubeID,
                     calTmin,
                     calTmax,
                     calP1,
                     calEreticCalibration,
                     ereticFactor,
                     temperature,
                     P1)
    return(df)
  } else {
    cat(crayon::yellow("fusion::readEretic >>", path, "not found\n"))
    return(NULL)
  }
}

