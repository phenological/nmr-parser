#' Extended Lipoproteins from 
#'
#' A dataset containing 112 Lipoproteins from IVDR method with additional extended parameters 
#' **calc** is from the summation of the raw values to find TL (total lipid) and
#' CH - FC to calculate CE.
#' **pct** looks at subfractions considering the lipid composition, so what
#' portion does each of the raw V1TG, V1CH, V1PL contribute to the calculated V1TL.
#' **frac** looks at subfractions considering the lipid distribution, so what
#' portion do each of V1TG, V2TG, V3TG, V4TG, V5TG contribute to VLTG
#'
#' \itemize{
#'   \item Fraction - lipoprotein fraction
#'   \item calc - total lipids and Cholesterol esters
#'   \item pct - sub-fraction lipid composition 
#'   \item frac - sub-fraction lipid distribution
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ext_lipo
#' @usage data(lipo)
#' @format A data frame with 112 lipoproteins values
data("ext_lipo", envir = asNamespace("nmr.parser"))
