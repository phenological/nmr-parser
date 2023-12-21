#
# # pathToLinks <- file.path("~/gitea/phenocare/rolodex/services/rolodex-nmr-links/nmr-links.sqlite")
# # links <- dbConnect(
# #   RSQLite::SQLite(),
# #   pathToLinks
# # )
# #
# # lof <- dbGetQuery(links, "select * from links where cohortName like 'F80Bruker'")
# # dbGetQuery(links, "select * from links where cohortName like 'F80Biogune'")
#
#
# dataPath <- "~/Downloads/BRUKER_600_80/80"
# sampleName <- dir(dataPath)
#
# listOfExperiments <- data.frame(projectName = "F80",
#                                 cohortName = "bruker-600",
#                                 sampleMatrixType = "SER",
#                                 runID = "EXTrXX",
#                                 sampleID = sampleName,
#                                 path = file.path(dataPath, sampleName, "10"))
# exp <- list()
# for (l in 1:nrow(listOfExperiments)) {
#   exp[[l]] <- readParam(file.path(listOfExperiments[l,]$path, "acqus"), "EXP")
# }
# exp <- do.call("rbind", exp)
#
# listOfExperiments$EXP <- exp
#
# ROLDX_DATA_PREFIX <- "~/Downloads/"
#
# listOfParams <- c('AQ_mod', 'AQSEQ', 'AUNM', 'AUTOPOS', 'BF1', 'BF2', 'BF3', 'BYTORDA', 'CNST_0', 'CNST_1', 'CNST_2', 'CNST_21', 'CNST_22', 'CNST_27', 'CNST_28', 'CNST_3', 'CNST_4', 'CPDPRG_0', 'CPDPRG_1', 'CPDPRG_2', 'CPDPRG_3', 'D_0', 'D_1', 'D_10', 'D_11', 'D_6', 'D_7', 'D_8', 'D_9', 'DATATYPE', 'DATE', 'DATE_START', 'DE', 'DECIM', 'DIGMOD', 'DIGTYP', 'DQDMODE', 'DSPFIRM', 'DSPFVS', 'DTYPA', 'EXP', 'FnMODE', 'FnTYPE', 'FQ1LIST', 'FQ2LIST', 'GRDPROG', 'GRPDLY', 'HOLDER', 'INSTRUM', 'L_0', 'L_1', 'L_2', 'L_4', 'LFILTER', 'LGAIN', 'LINPSTP', 'LOCKED', 'LOCKFLD', 'LOCKGN', 'LOCKPOW', 'LOCKPPM', 'LOCNUC', 'LOCPHAS', 'LOCSHFT', 'LOCSW', 'NC', 'NPOINTS', 'NS', 'NUC1', 'NUC2', 'NUC3', 'NUCLEUS', 'O1', 'O2', 'O3', 'ORIGIN', 'OVERFLW', 'OWNER', 'P_0', 'P_1', 'P_15', 'P_16', 'P_18', 'P_2', 'P_27', 'P_3', 'P_4', 'P_7', 'PARMODE', 'PH_ref', 'PHCOR_0', 'PHCOR_1', 'PHLIST', 'PHP', 'PL_0', 'PL_1', 'PL_11', 'PL_12', 'PL_13', 'PL_2', 'PL_7', 'PLSTEP', 'PLSTRT', 'PRGAIN', 'PROBHD', 'PULPROG', 'PW', 'RG', 'SFO1', 'SFO2', 'SFO3', 'SigLockShift', 'SOLVENT', 'SOLVOLD', 'SP_0', 'SP_1', 'SP_10', 'SP_11', 'SP_2', 'SP_8', 'SW', 'SW_h', 'SWfinal', 'TD', 'TD_INDIRECT_0', 'TD_INDIRECT_1', 'TD0', 'TDav', 'TE', 'TE_MAGNET', 'TE_PIDX', 'TE1', 'TE2', 'TE3', 'TE4', 'TEG', 'TITLE', 'TUBE_TYPE', 'USERA2', 'UUID', 'YMAX_a', 'YMIN_a', 'ZGOPTNS', 'instrument', 'instrumentDate', 'instrumentTime', 'instrumentTimezone')
#
# makeNmrDataElement(listOfExperiments)
#
# makeNmrDataElement <- function(loe, output = ".", options = list() ) {
#   from <- -0.1
#   to <- 10
#   length.out <- 44079
#   l_dataPath <- loe$path #loe$paths$paths$dataPath
#   l_EXP <- loe$EXP #loe$params$params$EXP
#   projectName <- loe$projectName[1] #fromJSON(loe$samples$samples$comments[[1]])$projectName
#   cohortName <- loe$cohortName[1] #fromJSON(loe$samples$samples$comments[[1]])$cohortName
#   sampleMatrixType <- loe$sampleMatrixType[1] #loe$samples$samples$matrix$matrix$matrix_name[[1]]
#
#   # check that there is only one type of experiments
#   if (length(table(l_EXP)) > 1) {
#     if ("file" %in% names(options)) {
#       fileName <- options$file
#     } else {
#       cat(crayon::red("makeNmrDataElement >> multiple EXP are found"))
#       stop("not recommended")
#     }
#   } else {
#     projectName <- projectName
#     cohortName <- cohortName
#     sampleMatrixType <- sampleMatrixType
#     EXP <- l_EXP[1]
#     runID <- loe$samples$samples$runID[1]
#
#     fileName <- paste(c(projectName,
#                         cohortName,
#                         sampleMatrixType,
#                         runID,
#                         gsub("_", ".", EXP)), collapse = "_")
#   }
#
#   # remove bruker calibration for PGPE
#   if (grepl("PGPE|DIRE", EXP)) {
#     uncalibrate <- TRUE
#     phasecheck<-TRUE # added new
#   } else {
#     uncalibrate <- FALSE
#     phasecheck<-FALSE # added new
#   }
#
#   spec <- info <- params <- sampls <- list()
#   ctr <- 1
#
#   for (i in 1:length(l_dataPath)) {
#     if (i%%100 == 0) {cat(i, "\r")}
#
#     # pathToData <- paste0(gsub(ROLDX_DATA_PREFIX,
#     #                           "02_data/nmr",
#     #                           l_dataPath[i]))
#     pathToData <- paste0(l_dataPath[i])
#
#     res <- readExperiment(pathToData, acqus = FALSE,
#                         options = list(fromTo = c(from, to),
#                                        length.out = length.out,
#                                        uncalibrate = uncalibrate,
#                                        eretic = TRUE))
#
#     # retrieve parameters and tests from nmr db
#     # route <- "/nmrParams"
#     # request <- paste0("/", loe$nmrFolderId[[i]])
#     # parms <- GET(paste0(ROLDX_URL, route, request))
#     # parms <- fromJSON(rawToChar(parms$content))
#     parms <- res$acqus
#     # idx <- res$acqus$name %in% listOfParams
#     # print(setdiff(listOfParams, res$acqus$name))
#     # parms <- parms[idx,]
#     parms <- reshape(parms, idvar = "path", timevar = "name", direction = "wide")
#
#     # retrieve samples information
#     # route <- "/samples"
#     # request <- paste0("/", loe$sampleId[[i]])
#     # sapls <- GET(paste0(ROLDX_URL, route, request))
#     # sapls <- fromJSON(rawToChar(sapls$content))
#     sapls <- loe[i,]$sampleID
#
#     spec[[ctr]] <- res$spec$spec$y
#     info[[ctr]] <- res$spec$info
#     params[[ctr]] <- parms
#     sampls[[ctr]] <- sapls
#     ctr <- ctr + 1
#   }
#   ppm <- res$spec$spec$x
#   newM <- do.call("rbind", spec)
#   newInfo <- do.call("rbind", info)
#   common_columns <- Reduce(intersect, lapply(params, names))
#   newParams <- data.frame(do.call("rbind", lapply(params, function(vec) vec[common_columns])))
#   newSamples <- data.frame(do.call("rbind", sampls))
#
#   cat(crayon::yellow("makeNmrDataElement >> found: ",
#                      nrow(newM),
#                      " spectra\n"))
#
#   info <- list(info = loe,
#                procs = newInfo,
#                params = newParams)
#
#   info[[1]]$sampleType <- "sample"
#   info[[1]]$sampleType[grepl("LTR", info[[1]]$sampleID)] <- "ltr"
#   info[[1]]$sampleType[grepl("QC", info[[1]]$sampleID)] <- "qc"
#   info[[1]]$sampleID <- unname(unlist(newSamples))
#   info[[1]]$sourceID <- unname(unlist(newSamples))
#   # info[[1]]$sampleTimePoint <- newSamples$sampleTimePoint
#   # info[[1]]$sampleAliquots <- newSamples$sampleAliquots
#
#   # info[[1]]$sampleID <- make.unique(info[[1]]$sampleID, sep = ".")
#   # ppm <- seq(from=-.1, to=10, length.out = ncol(newM))
#   # create dataElement
#   da <- new("dataElement",
#             .Data = newM,
#             obsDescr = info,
#             varName = as.character(ppm),
#             type = "NMR",
#             method = "NOESY")
#
#   fileName <- paste(c(projectName,
#                       cohortName,
#                       sampleMatrixType,
#                       runID,
#                       gsub("_", ".", EXP)), collapse = "_")
#
#   assign(fileName, da)
#
#   save(list=(fileName),
#        file = file.path(output, paste0(fileName, ".daE")))
#   # daE can be loaded as var <- local(get(load(daE))) to rename them on the fly
# }
