#' get names and description of lipoproteins IVDr parameters
#' @param extended if TRUE, the extended_lipo information is attached below the lipoTable
#' @return a data.frame with information
#' @importFrom utils data
#' @export
getLipoTable <- function(extended = FALSE, withDensities = FALSE) {
  lipo <- get0("lipo", envir = asNamespace("nmr.parser"))

  if (withDensities == TRUE) {
    lipoDensities <- get0("lipoWithDensities", envir = asNamespace("nmr.parser"))
    return(lipoDensities)
  }


  ext_lipo <- extend_lipo(lipo)


  ext_lipo$data$range <- paste0(ext_lipo$data$refMin, " - ", ext_lipo$data$refMax,
                                " (", ext_lipo$data$refUnit, ")")

  names(ext_lipo$data) <-c("Fraction",
                           "Compound",
                           "Abbreviation",
                           "ID",
                           "Type",
                           "Value",
                           "Unit",
                           "Max Value (ref.)",
                           "Min Value (ref.)",
                           "Reference Unit",
                           "tag",
                           "Reference Range [Unit]")

  # correcting typo in xml
  ext_lipo$data$Compound[9] <- "Apo-B100 / Apo-A1"
  rownames(ext_lipo$data) <- c(1:nrow(ext_lipo$data))
  if(!extended){

    return(ext_lipo$data[1:112, c("Fraction",
                            "Compound",
                            "ID",
                            "Reference Range [Unit]",
                            "tag")]
           )
  } else {
    return(ext_lipo$data[,c("Fraction",
                       "Compound",
                       "ID",
                       "Reference Range [Unit]",
                       "tag")]
    )
  }

}

