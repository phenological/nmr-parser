#' extend_lipo
#' calculate the total lipids and its percentage using data.frame "res" from readLipo.r
#' @param res - data frame with lipoprotein data
#' @return - a data frame with new parameters
#' @export
extend_lipo<-function(res){
  tdf<-data.frame(t(res$value))
  colnames(tdf)<-res$id
  tdf$calc.HDTL = tdf$HDTG+tdf$HDCH+tdf$HDPL
  tdf$calc.HDCE = tdf$HDCH-tdf$HDFC
  tdf$perc.HDTG = round(tdf$HDTG/tdf$calc.HDTL,4)*100
  tdf$perc.H1TG = round(tdf$H1TG/tdf$HDTG,4)*100
  tdf$perc.H2TG = round(tdf$H2TG/tdf$HDTG,4)*100
  tdf$perc.H3TG = round(tdf$H3TG/tdf$HDTG,4)*100
  tdf$perc.H4TG = round(tdf$H4TG/tdf$HDTG,4)*100
  tdf$perc.HDCH = round(tdf$HDCH/tdf$calc.HDTL,4)*100
  tdf$perc.H1CH = round(tdf$H1CH/tdf$HDCH,4)*100
  tdf$perc.H2CH = round(tdf$H2CH/tdf$HDCH,4)*100
  tdf$perc.H3CH = round(tdf$H3CH/tdf$HDCH,4)*100
  tdf$perc.H4CH = round(tdf$H4CH/tdf$HDCH,4)*100
  tdf$perc.HDFC = round(tdf$HDFC/tdf$calc.HDTL,4)*100
  tdf$perc.H1FC = round(tdf$H1FC/tdf$HDFC,4)*100
  tdf$perc.H2FC = round(tdf$H2FC/tdf$HDFC,4)*100
  tdf$perc.H3FC = round(tdf$H3FC/tdf$HDFC,4)*100
  tdf$perc.H4FC = round(tdf$H4FC/tdf$HDFC,4)*100
  tdf$perc.HDCE = round(tdf$calc.HDCE/tdf$calc.HDTL,4)*100
  tdf$perc.H1CE = round(c(tdf$H1CH-tdf$H1FC)/tdf$calc.HDCE,4)*100
  tdf$perc.H2CE = round(c(tdf$H2CH-tdf$H2FC)/tdf$calc.HDCE,4)*100
  tdf$perc.H3CE = round(c(tdf$H3CH-tdf$H3FC)/tdf$calc.HDCE,4)*100
  tdf$perc.H4CE = round(c(tdf$H4CH-tdf$H4FC)/tdf$calc.HDCE,4)*100
  tdf$perc.HDPL = round(tdf$HDPL/tdf$calc.HDTL,4)*100
  tdf$perc.H1PL = round(tdf$H1PL/tdf$HDPL,4)*100
  tdf$perc.H2PL = round(tdf$H2PL/tdf$HDPL,4)*100
  tdf$perc.H3PL = round(tdf$H3PL/tdf$HDPL,4)*100
  tdf$perc.H4PL = round(tdf$H4PL/tdf$HDPL,4)*100
  tdf$calc.VLTL = tdf$VLTG+tdf$VLCH+tdf$VLPL
  tdf$calc.VLCE = tdf$VLCH-tdf$VLFC
  tdf$perc.VLTG = round(tdf$VLTG/tdf$calc.VLTL,4)*100
  tdf$perc.V1TG = round(tdf$V1TG/tdf$VLTG,4)*100
  tdf$perc.V2TG = round(tdf$V2TG/tdf$VLTG,4)*100
  tdf$perc.V3TG = round(tdf$V3TG/tdf$VLTG,4)*100
  tdf$perc.V4TG = round(tdf$V4TG/tdf$VLTG,4)*100
  tdf$perc.V5TG = round(tdf$V5TG/tdf$VLTG,4)*100
  tdf$perc.VLCH = round(tdf$VLCH/tdf$calc.VLTL,4)*100
  tdf$perc.V1CH = round(tdf$V1CH/tdf$VLCH,4)*100
  tdf$perc.V2CH = round(tdf$V2CH/tdf$VLCH,4)*100
  tdf$perc.V3CH = round(tdf$V3CH/tdf$VLCH,4)*100
  tdf$perc.V4CH = round(tdf$V4CH/tdf$VLCH,4)*100
  tdf$perc.V5CH = round(tdf$V5CH/tdf$VLCH,4)*100
  tdf$perc.VLFC = round(tdf$VLFC/tdf$calc.VLTL,4)*100
  tdf$perc.V1FC = round(tdf$V1FC/tdf$VLFC,4)*100
  tdf$perc.V2FC = round(tdf$V2FC/tdf$VLFC,4)*100
  tdf$perc.V3FC = round(tdf$V3FC/tdf$VLFC,4)*100
  tdf$perc.V4FC = round(tdf$V4FC/tdf$VLFC,4)*100
  tdf$perc.V5FC = round(tdf$V5FC/tdf$VLFC,4)*100
  tdf$perc.VLCE = round(tdf$calc.VLCE/tdf$calc.VLTL,4)*100
  tdf$perc.V1CE = round(c(tdf$V1CH-tdf$V1FC)/tdf$calc.VLCE,4)*100
  tdf$perc.V2CE = round(c(tdf$V2CH-tdf$V2FC)/tdf$calc.VLCE,4)*100
  tdf$perc.V3CE = round(c(tdf$V3CH-tdf$V3FC)/tdf$calc.VLCE,4)*100
  tdf$perc.V4CE = round(c(tdf$V4CH-tdf$V4FC)/tdf$calc.VLCE,4)*100
  tdf$perc.V5CE = round(c(tdf$V5CH-tdf$V5FC)/tdf$calc.VLCE,4)*100
  tdf$perc.VLPL = round(tdf$VLPL/tdf$calc.VLTL,4)*100
  tdf$perc.V1PL = round(tdf$V1PL/tdf$VLPL,4)*100
  tdf$perc.V2PL = round(tdf$V2PL/tdf$VLPL,4)*100
  tdf$perc.V3PL = round(tdf$V3PL/tdf$VLPL,4)*100
  tdf$perc.V4PL = round(tdf$V4PL/tdf$VLPL,4)*100
  tdf$perc.V5PL = round(tdf$V5PL/tdf$VLPL,4)*100
  tdf$calc.IDTL = tdf$IDTG+tdf$IDCH+tdf$IDPL
  tdf$calc.IDCE = tdf$IDCH-tdf$IDFC
  tdf$perc.IDTG = round(tdf$IDTG/tdf$calc.IDTL,4)*100
  tdf$perc.IDCH = round(tdf$IDCH/tdf$calc.IDTL,4)*100
  tdf$perc.IDFC = round(tdf$IDFC/tdf$calc.IDTL,4)*100
  tdf$perc.IDCE = round(tdf$calc.IDCE/tdf$calc.IDTL,4)*100
  tdf$perc.IDPL = round(tdf$IDPL/tdf$calc.IDTL,4)*100
  tdf$calc.LDTL = tdf$LDTG+tdf$LDCH+tdf$LDPL
  tdf$calc.LDCE = tdf$LDCH-tdf$LDFC
  tdf$perc.LDTG = round(tdf$LDTG/tdf$calc.LDTL,4)*100
  tdf$perc.L1TG = round(tdf$L1TG/tdf$LDTG,4)*100
  tdf$perc.L2TG = round(tdf$L2TG/tdf$LDTG,4)*100
  tdf$perc.L3TG = round(tdf$L3TG/tdf$LDTG,4)*100
  tdf$perc.L4TG = round(tdf$L4TG/tdf$LDTG,4)*100
  tdf$perc.L5TG = round(tdf$L5TG/tdf$LDTG,4)*100
  tdf$perc.L6TG = round(tdf$L6TG/tdf$LDTG,4)*100
  tdf$perc.LDCH = round(tdf$LDCH/tdf$calc.LDTL,4)*100
  tdf$perc.L1CH = round(tdf$L1CH/tdf$LDCH,4)*100
  tdf$perc.L2CH = round(tdf$L2CH/tdf$LDCH,4)*100
  tdf$perc.L3CH = round(tdf$L3CH/tdf$LDCH,4)*100
  tdf$perc.L4CH = round(tdf$L4CH/tdf$LDCH,4)*100
  tdf$perc.L5CH = round(tdf$L5CH/tdf$LDCH,4)*100
  tdf$perc.L6CH = round(tdf$L6CH/tdf$LDCH,4)*100
  tdf$perc.LDFC = round(tdf$LDFC/tdf$calc.LDTL,4)*100
  tdf$perc.L1FC = round(tdf$L1FC/tdf$LDFC,4)*100
  tdf$perc.L2FC = round(tdf$L2FC/tdf$LDFC,4)*100
  tdf$perc.L3FC = round(tdf$L3FC/tdf$LDFC,4)*100
  tdf$perc.L4FC = round(tdf$L4FC/tdf$LDFC,4)*100
  tdf$perc.L5FC = round(tdf$L5FC/tdf$LDFC,4)*100
  tdf$perc.L6FC = round(tdf$L6FC/tdf$calc.LDTL,4)*100
  tdf$perc.LDCE = round(tdf$calc.LDCE/tdf$calc.LDTL,4)*100
  tdf$perc.L1CE = round(c(tdf$L1CH-tdf$L1FC)/tdf$calc.LDCE,4)*100
  tdf$perc.L2CE = round(c(tdf$L2CH-tdf$L2FC)/tdf$calc.LDCE,4)*100
  tdf$perc.L3CE = round(c(tdf$L3CH-tdf$L3FC)/tdf$calc.LDCE,4)*100
  tdf$perc.L4CE = round(c(tdf$L4CH-tdf$L4FC)/tdf$calc.LDCE,4)*100
  tdf$perc.L5CE = round(c(tdf$L5CH-tdf$L5FC)/tdf$calc.LDCE,4)*100
  tdf$perc.L6CE = round(c(tdf$L6CH-tdf$L6FC)/tdf$calc.LDCE,4)*100
  tdf$perc.LDPL = round(tdf$LDPL/tdf$calc.LDTL,4)*100
  tdf$perc.L1PL = round(tdf$L1PL/tdf$LDPL,4)*100
  tdf$perc.L2PL = round(tdf$L2PL/tdf$LDPL,4)*100
  tdf$perc.L3PL = round(tdf$L3PL/tdf$LDPL,4)*100
  tdf$perc.L4PL = round(tdf$L4PL/tdf$LDPL,4)*100
  tdf$perc.L5PL = round(tdf$L5PL/tdf$LDPL,4)*100
  tdf$perc.L6PL = round(tdf$L6PL/tdf$LDPL,4)*100
  tdf$calc.TBPN = tdf$VLPN+tdf$IDPN+tdf$L1PN+tdf$L2PN+tdf$L3PN+tdf$L4PN+tdf$L5PN+tdf$L6PN
  tdf$perc.VLPN = round(tdf$VLPN/tdf$calc.TBPN,4)*100
  tdf$perc.IDPN = round(tdf$IDPN/tdf$calc.TBPN,4)*100
  tdf$perc.L1PN = round(tdf$L1PN/tdf$calc.TBPN,4)*100
  tdf$perc.L2PN = round(tdf$L2PN/tdf$calc.TBPN,4)*100
  tdf$perc.L3PN = round(tdf$L3PN/tdf$calc.TBPN,4)*100
  tdf$perc.L4PN = round(tdf$L4PN/tdf$calc.TBPN,4)*100
  tdf$perc.L5PN = round(tdf$L5PN/tdf$calc.TBPN,4)*100
  tdf$perc.L6PN = round(tdf$L6PN/tdf$calc.TBPN,4)*100
  tdf<-tdf[,-which(colnames(tdf) %in% res$id)]
return(tdf)

}

