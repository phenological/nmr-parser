#' function to extend the lipo and return a vector of values
#' @param lipo - takes a lipo object
#' @return a vector with values and no particular format
#' @export

extend_lipo_value <- function(lipo){

  df <- as.data.frame(t(lipo$data$value))
  colnames(df) <- lipo$data$id

  #######calc###########
  #create the required column (all in the names already)
  calc <- data.frame(
    HDTL = df$HDTG + df$HDCH + df$HDPL,
    HDCE = df$HDCH - df$HDFC,
    VLTL = df$VLTG + df$VLCH + df$VLPL,
    VLCE = df$VLCH - df$VLFC,
    IDTL = df$IDTG + df$IDCH + df$IDPL,
    IDCE = df$IDCH - df$IDFC,
    LDTL = df$LDTG + df$LDCH + df$LDPL,
    LDCE = df$LDCH - df$LDFC,
    TBPN = df$VLPN + df$IDPN + df$L1PN + df$L2PN + df$L3PN + df$L4PN + df$L5PN + df$L6PN,
    HDA1 = df$H1A1 + df$H2A1 + df$H3A1 + df$H4A1,
    HDA2 = df$H1A2 + df$H2A2 + df$H3A2 + df$H4A2,
    LDAB = df$L1AB + df$L2AB + df$L3AB + df$L4AB + df$L5AB + df$L6AB,
    V1TL = df$V1TG + df$V1CH + df$V1PL,
    V2TL = df$V2TG + df$V2CH + df$V2PL,
    V3TL = df$V3TG + df$V3CH + df$V3PL,
    V4TL = df$V4TG + df$V4CH + df$V4PL,
    V5TL = df$V5TG + df$V5CH + df$V5PL,
    L1TL = df$L1TG + df$L1CH + df$L1PL,
    L2TL = df$L2TG + df$L2CH + df$L2PL,
    L3TL = df$L3TG + df$L3CH + df$L3PL,
    L4TL = df$L4TG + df$L4CH + df$L4PL,
    L5TL = df$L5TG + df$L5CH + df$L5PL,
    L6TL = df$L6TG + df$L6CH + df$L6PL,
    H1TL = df$H1TG + df$H1CH + df$H1PL,
    H2TL = df$H2TG + df$H2CH + df$H2PL,
    H3TL = df$H3TG + df$H3CH + df$H3PL,
    H4TL = df$H4TG + df$H4CH + df$H4PL
  )

  ############pct#######
  #create fracentage df use calc and df
  #initialisation (6 rows)
  pct <- data.frame(
    HDCE = round(calc$HDCE / calc$HDTL, 4) * 100,
    VLCE = round(calc$VLCE / calc$VLTL, 4) * 100,
    IDCE = round(calc$IDCE / calc$IDTL, 4) * 100,
    LDCE = round(calc$LDCE / calc$LDTL, 4) * 100,
    VLPN = round(df$VLPN / calc$TBPN, 4) * 100,
    IDPN = round(df$IDPN / calc$TBPN, 4) * 100
  )

  letters <- c("H", "V", "L")
  ranges <- list(H = 1:4, V = 1:5, L = 1:6)
  prefixes <- c("HD", "VL", "ID", "LD")
  suffixes <- c("TG", "CH", "FC", "PL")
  suffixes2 <- c("A1", "A2")

  #"H1CE" "H2CE" "H3CE" "H4CE" "V1CE" "V2CE" "V3CE" "V4CE" "V5CE" "L1CE" "L2CE" "L3CE" "L4CE" "L5CE" "L6CE"
  for (letter in letters) {
    for (i in ranges[[letter]]) {
      ch_col <- paste0(letter, i, "CH")
      fc_col <- paste0(letter, i, "FC")
      ce_col <- paste0(letter, i, "CE")
      calc_col <- paste0(letter, i, "TL")
      pct[[ce_col]] <- round((df[[ch_col]] - df[[fc_col]]) / calc[[calc_col]], 4) * 100
    }
  }

  #"H1TG" "H1FC"  "H1PL"  "H2TG"  "H2FC"  "H2PL"  "H3TG"  "H3FC"  "H3PL"  "H4TG"  "H4FC"  "H4PL"  "V1TG"  "V1FC"  "V1PL"  "V2TG"  "V2FC"  "V2PL"  "V3TG"
  # "V3FC"  "V3PL"  "V4TG"  "V4FC"  "V4PL"  "V5TG"  "V5FC"  "V5PL"  "L1TG"  "L1FC"  "L1PL"  "L2TG"  "L2FC"  "L2PL"  "L3TG"  "L3FC"  "L3PL"  "L4TG"
  # "L4FC"  "L4PL"  "L5TG"  "L5FC"  "L5PL"  "L6TG"  "L6FC"  "L6PL"
  for (letter in letters) {
    for (i in ranges[[letter]]) {
      for (suffix in suffixes) {
        if(suffix == "CH") next
        col <- paste0(letter, i, suffix)
        calc_col <- paste0(letter, i, "TL")
        pct[[col]] <- round(df[[col]] / calc[[calc_col]], 4) * 100
      }
    }
  }

  #"HDTG" "HDCH" "HDFC" "HDPL" "VLTG" "VLCH" "VLFC" "VLPL" "IDTG" "IDCH" "IDFC" "IDPL" "LDTG" "LDCH" "LDFC" "LDPL"
  for (prefix in prefixes) {
    for (suffix in suffixes) {
      col <- paste0(prefix, suffix)
      calc_col <- paste0(prefix, "TL")
      pct[[col]] <- round(df[[col]] / calc[[calc_col]], 4) * 100
    }
  }

  #########frac########
  frac <- data.frame(row.names = seq_len(nrow(df)))

  #"H1CE" "H2CE" "H3CE" "H4CE" "V1CE" "V2CE" "V3CE" "V4CE" "V5CE" "L1CE" "L2CE" "L3CE" "L4CE" "L5CE" "L6CE"
  for (letter in letters) {
    for (i in ranges[[letter]]) {
      ch_col <- paste0(letter, i, "CH")
      fc_col <- paste0(letter, i, "FC")
      ce_col <- paste0(letter, i, "CE")
      calc_col <- if (letter == "V") "VLCE" else paste0(letter, "DCE")
      frac[[ce_col]] <- round((df[[ch_col]] - df[[fc_col]]) / calc[[calc_col]], 4) * 100
    }
  }

  #"H1TG" "H1CH" "H1FC" "H1PL" "H2TG" "H2CH" "H2FC" "H2PL" "H3TG" "H3CH" "H3FC" "H3PL" "H4TG" "H4CH" "H4FC" "H4PL" "V1TG" "V1CH" "V1FC" "V1PL" "V2TG" "V2CH" "V2FC" "V2PL" "V3TG"
  #"V3CH" "V3FC" "V3PL" "V4TG" "V4CH" "V4FC" "V4PL" "V5TG" "V5CH" "V5FC" "V5PL" "L1TG" "L1CH" "L1FC" "L1PL" "L2TG" "L2CH" "L2FC" "L2PL" "L3TG" "L3CH" "L3FC" "L3PL"
  #"L4TG" "L4CH" "L4FC" "L4PL" "L5TG" "L5CH" "L5FC" "L5PL" "L6TG" "L6CH" "L6FC" "L6PL"

  #note that these values are calculated using only the raw data (df not calc denominator), this is still acceptable since, for example, V1PL_frac can be calculated using V1PL_raw/VLPL_raw
  #OR V1PL_raw/(V1PL_raw + V2PL_raw + V4PL_raw + V4PL_raw + V5PL_raw). The denominators should be the same (the raw should be the sum of V1 to V5), sometimes there is a slight discrepancy
  #between the raw and adding these parts but it is only slight and due to experimental error
  for (letter in letters) {
    for (i in ranges[[letter]]) {
      for (suffix in suffixes) {
        for (prefix in prefixes){
          if (prefix == "ID") next  # Skip "ID"
          col <- paste0(letter, i, suffix)
          col2 <- paste0(prefix, suffix)
          frac[[col]] <- round(df[[col]] / df[[col2]], 4) * 100
        }
      }
    }
  }

  #"H1A1" "H1A2" "H2A1" "H2A2" "H3A1" "H3A2" "H4A1" "H4A2"
  for (i in ranges[["H"]]) {
    for (suffix in suffixes2) {
      col <- paste0("H", i, suffix)
      calc_col <- paste0("HD", suffix)
      frac[[col]] <- round(df[[col]] / calc[[calc_col]], 4) * 100
    }
  }

  #"L1AB" "L1PN" "L2AB" "L2PN" "L3AB" "L3PN" "L4AB" "L4PN" "L5AB" "L5PN" "L6AB" "L6PN"
  for (i in ranges[["L"]]) {
    col <- paste0("L", i, "AB")
    calc_col <- paste0("LDAB")
    frac[[col]] <- round(df[[col]] / calc[[calc_col]], 4) * 100

    col <- paste0("L", i, "PN")
    calc_col <- paste0("TBPN")
    frac[[col]] <- round(df[[col]] / calc[[calc_col]], 4) * 100
  }

  #column names
  #calc
  colnames(calc) <- paste(colnames(calc), "calc", sep = "_")

  #pct
  colnames(pct) <- paste(colnames(pct), "pct", sep = "_")

  #frac
  colnames(frac) <- paste(colnames(frac), "frac", sep = "_")

  df <- cbind(df, calc, pct, frac)

}

#' extend_lipo
#' Calculates total lipids, fractions and percentages using data.frame "res"
#' from readLipo.r
#' @param lipo Data frame with lipoprotein data for a single sample
#' @return Data frame with new parameters.
#' **calc** is from the summation of the raw values to find TL (total lipid) and
#' CH - FC to calculate CE.
#' **pct** looks at subfractions considering the lipid composition, so what
#' portion does each of the raw V1TG, V1CH, V1PL contribute to the calculated V1TL.
#' **frac** looks at subfractions considering the lipid distribution, so what
#' portion do each of V1TG, V2TG, V3TG, V4TG, V5TG contribute to VLTG
#' @export
extend_lipo <- function(lipo) {


  df <- extend_lipo_value(lipo)

  extended_lipo_vector <- t(df)

  df <- data.frame(id = rownames(extended_lipo_vector), value = extended_lipo_vector)
  # expand lipo table to fit extra parameters from extend_lipo
  df <- lipo$data[match(df$id, lipo$data$id),]
  id <- row.names(extended_lipo_vector)
  df$id <- id
  value <- as.numeric(extended_lipo_vector)
  df$value <- value

  # refMax
  ma <- lipo
  ma$data$value <- lipo$data$refMax
  ma <- extend_lipo_value(ma)

  # refMin
  mi <- lipo
  mi$data$value <- lipo$data$refMin
  mi <- extend_lipo_value(mi)



  df$refMax <- ifelse(ma < mi, mi, ma)
  df$refMin <- ifelse(ma > mi, mi, ma)

  ## Fraction ##
  df$fraction<-ifelse(is.na(df$fraction),
                            lipo$data$fraction[match(gsub("CE","CH",substr(df$id,1,4)),lipo$data$id)],
                            df$fraction)


  df$fraction<-ifelse(is.na(df$fraction),
                            lipo$data$fraction[match(substr(df$id,1,2),substr(lipo$data$id,1,2))],
                            df$fraction)
  ## name ##
  df$name<-ifelse(is.na(df$name),
                        lipo$data$name[match(substr(df$id,1,4),lipo$data$id)],
                        df$name)
  df$name<-ifelse(grepl("CE",df$id),
                        "Cholesterol Ester",
                        df$name)
  df$name<-ifelse(is.na(df$name) & grepl("TL",df$id),
                        c("Triglycerides, Cholesterol, Phospholipids"),
                        df$name)
  ## abbr ##
  df$abbr<-ifelse(is.na(df$abbr),
                        lipo$data$abbr[match(gsub("CE","CH",substr(df$id,1,4)),lipo$data$id)],
                        df$abbr)

  df$abbr<-ifelse(is.na(df$type) & grepl("CE",df$id) & !is.na(df$abbr),
                        gsub("-Chol","-CE",df$abbr),
                        df$abbr)

  df$abbr<-ifelse(is.na(df$abbr),
                        lipo$data$abbr[match(gsub("TL","TG",substr(df$id,1,4)),lipo$data$id)],
                        df$abbr)
  ## type ##

  df$type<-"prediction"

  ## unit ##
  df$unit <- ifelse(grepl("calc",df$id) & df$id!="TBPN_calc","mg/dL",df$unit)
  df$unit <- ifelse(df$id=="TBPN_calc","nmol/L",df$unit)
  df$unit <- ifelse(is.na(df$unit),"-/-",df$unit)
  ## refUnit ##
  df$refUnit<-df$unit

  # df$range <- paste0(df$refMin, " - ", df$refMax,
  #                          " (", df$refUnit, ")")
  names(df) <-c("fraction",
                      "name",
                      "abbr",
                      "id",
                      "type",
                      "value",
                      "unit",
                      "refMax",
                      "refMin",
                      "refUnit")
  # correcting typo in xml
  df$name[9] <- "Apo-B100 / Apo-A1"
  rownames(df) <- c(1:nrow(df))
  # cleaning column text
  #df$Compound <- gsub(" ", "", df$Compound)
  df$abbr <- gsub(" ", "", df$abbr)

  # creating label for publication
  df$tag <- paste0(df$name, ", ", df$abbr)
  df$tag[grep("TPTG",df$id)]<- "Triglycerides, total"
  df$tag[grep("TPCH",df$id)] <- "Cholesterol, total"
  df$tag[grep("LDCH",df$id)] <- "Cholesterol, LDL"
  df$tag[grep("HDCH",df$id)]  <- "Cholesterol, HDL"
  df$tag[grep("TPA1",df$id)]  <- "Apo-A1, total"
  df$tag[grep("TPA2",df$id)] <- "Apo-A2, total"
  df$tag[grep("TPAB",df$id)] <- "Apo-B100, total"

  df$tag[grep("LDHD",df$id)]  <- "LDL-Chol/HDL-Chol"
  df$tag[grep("ABA1",df$id)]  <- "Apo-B100/Apo-A1"
  df$tag[grep("TBPN",df$id)]  <- "Apo-B100, particle number"
  df$tag[grep("VLPN",df$id)]  <- "VLDL, particle number"
  df$tag[grep("IDPN",df$id)]  <- "IDL, particle number"
  df$tag[grep("LDPN",df$id)]  <- "LDL, particle number"
  df$tag[grep("L1PN",df$id)]  <- "LD1, particle number"
  df$tag[grep("L2PN",df$id)]  <- "LD2, particle number"
  df$tag[grep("L3PN",df$id)]  <- "LD3, particle number"
  df$tag[grep("L4PN",df$id)]  <- "LD4, particle number"
  df$tag[grep("L5PN",df$id)]  <- "LD5, particle number"
  df$tag[grep("L6PN",df$id)]  <- "LD6, particle number"


  return(list(data = df, version = lipo$version))
}


