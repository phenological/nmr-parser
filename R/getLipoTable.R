#' get names and description of lipoproteins IVDr parameters
#' @param extended if TRUE, the extended_lipo information is attached below the lipoTable
#' @return a data.frame with information
#' @importFrom utils data
#' @export
getLipoTable <- function(extended = FALSE) {
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
  if(!extended){

    return(lipo[, c("Fraction",
                    "Compound",
                    "ID",
                    "Reference Range [Unit]",
                    "tag")])

  }else{
    t <- lipo[,c("Value","ID")]
    names(t) <- c("value","id")
    t <- nmr.parser::extend_lipo(t)
    t <- t(t)
    ma <- lipo[,c("Max Value (ref.)","ID")]
    names(ma) <- c("value","id")
    ma <- nmr.parser::extend_lipo(ma)
    mi <- lipo[,c("Min Value (ref.)","ID")]
    names(mi) <- c("value","id")
    mi <- nmr.parser::extend_lipo(mi)

    ext_lipo <- data.frame(
                         Fraction = substr(sapply(strsplit(rownames(t),"_",fixed = T),"[",1), 1, 2),
                         Compound = substr(sapply(strsplit(rownames(t),"_",fixed = T),"[",1), 3, 4),
                         Abbreviation = NA,
                         ID = rownames(t),
                         maxRef = t(ma),
                         minRef = t(mi),
                         refUnit = factor(sapply(strsplit(rownames(t),"_",fixed = T),"[",2),
                                                   levels = c(NA, "calc", "pct", "frac"),
                                                   labels = c("mg/dL", "%", "a.u.")),
                         check.names = FALSE)

    row.names(ext_lipo) = seq(1,317)

    ext_lipo$`Reference Range [Unit]` = ifelse(ext_lipo$minRef < ext_lipo$maxRef,
                                               paste0( ext_lipo$minRef,
                                                       " - ",
                                                       ext_lipo$maxRef,
                                                       " (",
                                                       ext_lipo$refUnit,
                                                       ")"),
                                               paste0( ext_lipo$maxRef,
                                                       " - ",
                                                       ext_lipo$minRef,
                                                       " (",
                                                       ext_lipo$refUnit,
                                                       ")"))

    ext_lipo$Fraction[grep("TL",ext_lipo$ID)]<-"Main Parameters"
    ext_lipo$Compound[grep("CE",ext_lipo$ID)]<-"Cholesterol Ester"
    ext_lipo$Compound[grep("TL",ext_lipo$ID)]<-"Triglycerides, Cholesterol, Phospholipids"
    ext_lipo$Abbreviation[grep("CE",ext_lipo$ID)]<-gsub("-Chol","",ext_lipo$Abbreviation[grep("CE",ext_lipo$ID)])
    ext_lipo$Abbreviation[grep("HDTL",ext_lipo$ID)] = "HDL"
    ext_lipo$Abbreviation[grep("VLTL",ext_lipo$ID)] = "VLDL"
    ext_lipo$Abbreviation[grep("IDTL",ext_lipo$ID)] = "IDL"
    ext_lipo$Abbreviation[grep("LDTL",ext_lipo$ID)] = "LDL"
    ext_lipo$tag<-paste0(ext_lipo$Compound,", ",ext_lipo$Abbreviation)
    names(ext_lipo) <- names(lipo)[-c(5,6,7)]

    return(ext_lipo[,c("Fraction","Compound","ID","Reference Range [Unit]","tag" )])
  }

}

