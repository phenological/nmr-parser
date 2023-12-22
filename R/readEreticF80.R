#' extract xml quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the title
#'
#' @export
#' @importFrom xml2 read_xml xml_text
readEreticF80 <- function(path){
  path <- file.path(path)
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    refName <- xml_text(xml_find_first(xml, "//Reference/refname"))
    refFile <- xml_text(xml_find_first(xml, "//Reference/file"))
    refAcquDate <- xml_text(xml_find_first(xml, "//Reference/Acquisition_date"))
    refProcDate <- xml_text(xml_find_first(xml, "//Reference/Processing_date"))
    refRegions <- xml_text(xml_find_first(xml, "//Reference/Regions"))
    refOneMolInt <- as.numeric(xml_text(xml_find_first(xml, "//Reference/one_mol_integral")))
    refOneMolPerReg <- as.numeric(xml_text(xml_find_first(xml, "//Reference/one_mol_per_region")))
    refD1 <- as.numeric(xml_text(xml_find_first(xml, "//Reference/D1")))
    refP1 <- as.numeric(xml_text(xml_find_first(xml, "//Reference/P1")))
    refRG <- as.numeric(xml_text(xml_find_first(xml, "//Reference/RG")))
    refNS <- as.numeric(xml_text(xml_find_first(xml, "//Reference/NS")))
    refSW <- as.numeric(xml_text(xml_find_first(xml, "//Reference/SW")))
    refSI <- as.numeric(xml_text(xml_find_first(xml, "//Reference/SI")))
    refOFFSET <- as.numeric(xml_text(xml_find_first(xml, "//Reference/OFFSET")))
    refPL9 <- as.numeric(xml_text(xml_find_first(xml, "//Reference/PL9")))
    refPL1 <- as.numeric(xml_text(xml_find_first(xml, "//Reference/PL1")))
    refDS <- as.numeric(xml_text(xml_find_first(xml, "//Reference/DS")))
    refTE <- as.numeric(xml_text(xml_find_first(xml, "//Reference/TE")))
    refD8 <- as.numeric(xml_text(xml_find_first(xml, "//Reference/D8")))
    refPROBHD <- gsub("<|>", "", xml_text(xml_find_first(xml, "//Reference/PROBHD")))

    samFile <- xml_text(xml_find_first(xml, "//Sample/file"))
    samAcquDate <- xml_text(xml_find_first(xml, "//Sample/Acquisition_date"))
    samProcDate <- xml_text(xml_find_first(xml, "//Sample/Processing_date"))
    samOneMolInt <- as.numeric(xml_text(xml_find_first(xml, "//Sample/one_mol_integral")))
    samD1 <- as.numeric(xml_text(xml_find_first(xml, "//Sample/D1")))
    samP1 <- as.numeric(xml_text(xml_find_first(xml, "//Sample/P1")))
    samRG <- as.numeric(xml_text(xml_find_first(xml, "//Sample/RG")))
    samNS <- as.numeric(xml_text(xml_find_first(xml, "//Sample/NS")))
    samSW <- as.numeric(xml_text(xml_find_first(xml, "//Sample/SW")))
    samSI <- as.numeric(xml_text(xml_find_first(xml, "//Sample/SI")))
    samOFFSET <- as.numeric(xml_text(xml_find_first(xml, "//Sample/OFFSET")))
    samPL9 <- as.numeric(xml_text(xml_find_first(xml, "//Sample/PL9")))
    samPL1 <- as.numeric(xml_text(xml_find_first(xml, "//Sample/PL1")))
    samDS <- as.numeric(xml_text(xml_find_first(xml, "//Sample/DS")))
    samTE <- as.numeric(xml_text(xml_find_first(xml, "//Sample/TE")))
    samD8 <- as.numeric(xml_text(xml_find_first(xml, "//Sample/D8")))
    samPROBHD <- gsub("<|>", "", xml_text(xml_find_first(xml, "//Sample/PROBHD")))

    dt <- data.table(field = 80,
                     RefName,
                     refFile,
                     refAcquDate,
                     refProcDate,
                     refRegions,
                     refOneMolInt,
                     refOneMolPerReg,
                     refD1,
                     refP1,
                     refRG,
                     refNS,
                     refSW,
                     refSI,
                     refOFFSET,
                     refPL9,
                     refPL1,
                     refDS,
                     refTE,
                     refD8,
                     refPROBHD,
                     samFile,
                     samAcquDate,
                     samProcDate,
                     samOneMolInt,
                     samD1,
                     samP1,
                     samRG,
                     samNS,
                     samSW,
                     samSI,
                     samOFFSET,
                     samPL9,
                     samPL1,
                     samDS,
                     samTE,
                     samD8,
                     samPROBHD
                     )
    return(dt)
  } else {
    cat(crayon::yellow("fusion::readEreticF80 >>", path, "not found\n"))
    return(NULL)
  }
}

