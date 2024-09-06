lipo <- get0("lipo", envir = asNamespace("nmr.parser"))
t<-nmr.parser::extend_lipo(lipo)
if(length(which(colnames(t)=="test_frac"))>0){
  t<-t[,-which(colnames(t)=="test_frac")]
}
t<-data.frame(id = rownames(t(t)),value = t(t))
# expand lipo table to fit extra parameters from extend_lipo
ext_lipo<-lipo[match(t$id,lipo$id),]
ext_lipo$id<-t$id
ext_lipo$value<-t$value

# refMax
ma <- lipo[,c("refMax","id")]
names(ma) <- c("value","id")
ma <- nmr.parser::extend_lipo(ma)
if(length(which(colnames(ma)=="test_frac"))>0){
  ma<-ma[,-which(colnames(ma)=="test_frac")]
}
ma<-data.frame(id = rownames(t(ma)),refMax = t(ma))
# refMax
mi <- lipo[,c("refMin","id")]
names(mi) <- c("value","id")
mi <- nmr.parser::extend_lipo(mi)
if(length(which(colnames(mi)=="test_frac"))>0){
  mi<-mi[,-which(colnames(mi)=="test_frac")]
}
mi<-data.frame(id = rownames(t(mi)),refMin = t(mi))
ext_lipo$refMax<-ifelse(ma$refMax<mi$refMin,mi$refMin,ma$refMax)
ext_lipo$refMin<-ifelse(ma$refMax>mi$refMin,mi$refMin,ma$refMax)
rm(t,ma,mi)

## Fraction ##
ext_lipo$fraction<-ifelse(is.na(ext_lipo$fraction),
                      lipo$fraction[match(gsub("CE","CH",substr(ext_lipo$id,1,4)),lipo$id)],
                      ext_lipo$fraction)
ext_lipo$fraction<-ifelse(is.na(ext_lipo$fraction),
                          lipo$fraction[match(substr(ext_lipo$id,1,2),substr(lipo$id,1,2))],
                          ext_lipo$fraction)
## name ##
ext_lipo$name<-ifelse(is.na(ext_lipo$name),
                          lipo$name[match(substr(ext_lipo$id,1,4),lipo$id)],
                          ext_lipo$name)
ext_lipo$name<-ifelse(grepl("CE",ext_lipo$id),
                      "Cholesterol Ester",
                      ext_lipo$name)
ext_lipo$name<-ifelse(is.na(ext_lipo$name) & grepl("TL",ext_lipo$id),
                      c("Triglycerides, Cholesterol, Phospholipids"),
                      ext_lipo$name)
## abbr ##
ext_lipo$abbr<-ifelse(is.na(ext_lipo$abbr),
                      lipo$abbr[match(gsub("CE","CH",substr(ext_lipo$id,1,4)),lipo$id)],
                      ext_lipo$abbr)

ext_lipo$abbr<-ifelse(is.na(ext_lipo$type) & grepl("CE",ext_lipo$id) & !is.na(ext_lipo$abbr),
                      gsub("-Chol","-CE",ext_lipo$abbr),
                      ext_lipo$abbr)

ext_lipo$abbr<-ifelse(is.na(ext_lipo$abbr),
                      lipo$abbr[match(gsub("TL","TG",substr(ext_lipo$id,1,4)),lipo$id)],
                      ext_lipo$abbr)
## type ##

ext_lipo$type<-"prediction"

## unit ##
ext_lipo$unit<-ifelse(grepl("calc",ext_lipo$id) & ext_lipo$id!="TBPN_calc","mg/dL",ext_lipo$unit)
ext_lipo$unit<-ifelse(ext_lipo$id=="TBPN_calc","nmol/L",ext_lipo$unit)
ext_lipo$unit<-ifelse(is.na(ext_lipo$unit),"-/-",ext_lipo$unit)
## refUnit ##
ext_lipo$refUnit<-ext_lipo$unit

ext_lipo$range <- paste0(ext_lipo$refMin, " - ", ext_lipo$refMax,
                     " (", ext_lipo$refUnit, ")")
names(ext_lipo) <-c("Fraction",
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
ext_lipo$Compound[9] <- "Apo-B100 / Apo-A1"
rownames(ext_lipo) <- c(1:nrow(ext_lipo))
# cleaning column text
ext_lipo$Compound <- gsub(" ", "", ext_lipo$Compound)
ext_lipo$Abbreviation <- gsub(" ", "", ext_lipo$Abbreviation)

# creating label for publication
ext_lipo$tag <- paste0(ext_lipo$Compound, ", ", ext_lipo$Abbreviation)
ext_lipo$tag[grep("TPTG",ext_lipo$ID)]<- "Triglycerides, total"
ext_lipo$tag[grep("TPCH",ext_lipo$ID)] <- "Cholesterol, total"
ext_lipo$tag[grep("LDCH",ext_lipo$ID)] <- "Cholesterol, LDL"
ext_lipo$tag[grep("HDCH",ext_lipo$ID)]  <- "Cholesterol, HDL"
ext_lipo$tag[grep("TPA1",ext_lipo$ID)]  <- "Apo-A1, total"
ext_lipo$tag[grep("TPA2",ext_lipo$ID)] <- "Apo-A2, total"
ext_lipo$tag[grep("TPAB",ext_lipo$ID)] <- "Apo-B100, total"

ext_lipo$tag[grep("LDHD",ext_lipo$ID)]  <- "LDL-Chol/HDL-Chol"
ext_lipo$tag[grep("ABA1",ext_lipo$ID)]  <- "Apo-B100/Apo-A1"
ext_lipo$tag[grep("TBPN",ext_lipo$ID)]  <- "Apo-B100, particle number"
ext_lipo$tag[grep("VLPN",ext_lipo$ID)]  <- "VLDL, particle number"
ext_lipo$tag[grep("IDPN",ext_lipo$ID)]  <- "IDL, particle number"
ext_lipo$tag[grep("LDPN",ext_lipo$ID)]  <- "LDL, particle number"
ext_lipo$tag[grep("L1PN",ext_lipo$ID)]  <- "LD1, particle number"
ext_lipo$tag[grep("L2PN",ext_lipo$ID)]  <- "LD2, particle number"
ext_lipo$tag[grep("L3PN",ext_lipo$ID)]  <- "LD3, particle number"
ext_lipo$tag[grep("L4PN",ext_lipo$ID)]  <- "LD4, particle number"
ext_lipo$tag[grep("L5PN",ext_lipo$ID)]  <- "LD5, particle number"
ext_lipo$tag[grep("L6PN",ext_lipo$ID)]  <- "LD6, particle number"


usethis::use_data(ext_lipo, overwrite = TRUE)
