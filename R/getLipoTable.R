#' get names and description of lipoproteins IVDr parameters
#' @return a data.frame with information
#' @importFrom utils data
#' @export
getLipoTable <- function() {
  lipo <- get0("lipo", envir = asNamespace("nmr.parser"))
  lipo$range <- paste0(lipo$refMin, " - ", lipo$refMax,
                       " (", lipo$refUnit, ")")
  names(lipo) <-c("Fraction",
                  "Compound",
                  "Abbreviation",
                  "ID",
                  "Type",
                  "Value",
                  "Unit",
                  "Max Value (ref.)",
                  "Min Value (ref.)",
                  "Reference Unit",
                  "Reference Range [Unit]")
  # correcting typo in xml
  lipo$Compound[9] <- "Apo-B100 / Apo-A1"
  rownames(lipo) <- c(1:nrow(lipo))

  # cleaning column text
  lipo$Compound <- gsub(" ", "", lipo$Compound)
  lipo$Abbreviation <- gsub(" ", "", lipo$Abbreviation)

  # creating label for publication
  tag <- paste0(lipo$Compound, ", ", lipo$Abbreviation)
  tag[1] <- "Triglycerides, total"
  tag[2] <- "Cholesterol, total"
  tag[3] <- "Cholesterol, LDL"
  tag[4] <- "Cholesterol, HDL"
  tag[5] <- "Apo-A1, total"
  tag[6] <- "Apo-A2, total"
  tag[7] <- "Apo-B100, total"

  tag[8] <- "LDL-Chol/HDL-Chol"
  tag[9] <- "Apo-B100/Apo-A1"
  tag[10] <- "Apo-B100, particle number"
  tag[11] <- "VLDL, particle number"
  tag[12] <- "IDL, particle number"
  tag[13] <- "LDL, particle number"
  tag[14] <- "LD1, particle number"
  tag[15] <- "LD2, particle number"
  tag[16] <- "LD3, particle number"
  tag[17] <- "LD4, particle number"
  tag[18] <- "LD5, particle number"
  tag[19] <- "LD6, particle number"

  lipo$tag <- tag

  return(lipo[,c(1,2,4,11, 12)])
}
