
test_that("reading plasma_quant_report_2_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report_2_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Ethanol")
  expect_equal(quant$data$rawConc[1], "0.624")  
  expect_equal(nrow(quant$data), 41)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "2.1.0+release.QuantPS_2.1.b9ea40f Research Use Only")
})

test_that("reading plasma_quant_report.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "plasma_quant_report.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Ethanol")
  expect_equal(quant$data$rawConc[1], "0.043")
  expect_equal(quant$data$sigCorr[1], "50")
  expect_equal(nrow(quant$data), 41)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-PS 2.0.0  Reseach Use Only")
})

test_that("reading urine_quant_report_b_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_b_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 50)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR B.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_b.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_b.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 50)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR B.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_e_1_2_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e_1_2_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "3.307")
  expect_equal(quant$data$sigCorr[1], '100')
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "1.2.0+release.QuantUR_E_1.2.f3ad700 Research Use Only")
})

test_that("reading urine_quant_report_e_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR E.1.1.0  Reseach Use Only")
})


test_that("reading urine_quant_report_e.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_e.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR E.1.1.0  Reseach Use Only")
})


test_that("reading urine_quant_report_ne_ver_1_0.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_ne_ver_1_0.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.7")
  expect_equal(quant$data$sigCorr[1], NA)
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR NE.1.1.0  Reseach Use Only")
})

test_that("reading urine_quant_report_ne.xml file", {
  quant <- readQuant(system.file("..", "tests", "testthat", "urine_quant_report_ne.xml", package = "nmr.parser"))
  expect_equal(quant$data$name[1], "Creatinine")
  expect_equal(quant$data$rawConc[1], "6.717")
  expect_equal(quant$data$sigCorr[1], "100")
  expect_equal(nrow(quant$data), 150)
  expect_equal(ncol(quant$data), 22)
  expect_equal(quant$version, "Quant-UR NE.1.1.0  Reseach Use Only")
})


test_that("a parameter keeps its own values when a sibling lacks a node", {
  #every attribute used to be swept from the whole document and the lists
  #lined up by position, so a PARAMETER carrying fewer children than its
  #neighbours shifted all the later ones onto the wrong compound. The six
  #_vr vectors even had a hard coded c(NA, ...) pad for it, correct only
  #while the parameter without a VALUERELATIVE is the first one
  report <- file.path(tempdir(), "gappy_quant_report.xml")
  on.exit(unlink(report), add = TRUE)

  parameter <- function(name, conc, rel, ref) paste0(
    '<PARAMETER name="', name, '" type="quantification">',
    '<VALUE conc="', conc, '" concUnit="mmol/L" lod="-" lodUnit="mmol/L" loq="-" loqUnit="mmol/L"/>',
    if (rel) paste0('<VALUERELATIVE conc="', conc, '9" concUnit="mmol/mol" lod="-" lodUnit="-" loq="-" loqUnit="-"/>') else "",
    '<RELDATA errConc="-" errConcUnit="mmol/L" rawConc="', conc, '5" rawConcUnit="mmol/L" sigCorr="100" sigCorrUnit="%"/>',
    if (ref) '<REFERENCE source="NMR Database" unit="mmol/L" vmax="9" vmin="0"/>' else "",
    '</PARAMETER>')

  writeLines(c('<?xml version="1.0" encoding="utf-8"?>',
               '<RESULTS version="Quant-UR B.1.1.0  Reseach Use Only">',
               '<QUANTIFICATION version="Quant-UR B.1.1.0  Reseach Use Only">',
               #the middle one carries neither a VALUERELATIVE nor a REFERENCE
               parameter("Alpha", 1, TRUE,  TRUE),
               parameter("Beta",  2, FALSE, FALSE),
               parameter("Gamma", 3, TRUE,  TRUE),
               '</QUANTIFICATION>',
               '</RESULTS>'), report)

  quant <- readQuant(report)

  expect_equal(quant$data$name, c("Alpha", "Beta", "Gamma"))

  #each compound keeps its own numbers
  expect_equal(quant$data$conc_v, c("1", "2", "3"))
  expect_equal(quant$data$rawConc, c("15", "25", "35"))

  #and the gaps land on the compound that has them, not on its neighbour
  expect_equal(quant$data$conc_vr, c("19", NA, "39"))
  expect_equal(quant$data$refMax, c("9", NA, "9"))
  expect_equal(quant$data$refMin, c("0", NA, "0"))
})

test_that("the document decides which shape it is, not the file name", {
  #Bruker gives both shapes the same version attribute, so the file name
  #used to pick the branch. Renaming a report then read it with the wrong
  #parser, which returned every compound with a NA concentration
  fixtures <- c("tests" = "urine_quant_report_b_ver_1_0.xml",
                "plain" = "urine_quant_report_b.xml")

  renamed <- file.path(tempdir(), c("urine_quant_report_b_renamed.xml",
                                    "urine_quant_report_b_ver_renamed.xml"))
  on.exit(unlink(renamed), add = TRUE)

  #the extended document under a name that says it is not, and vice versa
  file.copy(system.file("..", "tests", "testthat", fixtures[["tests"]], package = "nmr.parser"),
            renamed[1], overwrite = TRUE)
  file.copy(system.file("..", "tests", "testthat", fixtures[["plain"]], package = "nmr.parser"),
            renamed[2], overwrite = TRUE)

  original <- lapply(fixtures, function(f)
    readQuant(system.file("..", "tests", "testthat", f, package = "nmr.parser"))$data)

  expect_equal(readQuant(renamed[1])$data, original[["tests"]])
  expect_equal(readQuant(renamed[2])$data, original[["plain"]])

  #and the concentrations really are there, not a frame full of NA
  expect_false(all(is.na(readQuant(renamed[1])$data$conc_v)))
  expect_false(all(is.na(readQuant(renamed[2])$data$conc_v)))
})

test_that("a document of neither shape is refused rather than guessed at", {
  report <- file.path(tempdir(), "not_a_quant_report.xml")
  on.exit(unlink(report), add = TRUE)
  writeLines(c('<?xml version="1.0" encoding="utf-8"?>',
               '<RESULTS version="Quant-UR B.1.1.0  Reseach Use Only">',
               '<QUANTIFICATION version="Quant-UR B.1.1.0  Reseach Use Only">',
               '<PARAMETER name="Alpha" type="quantification"/>',
               '</QUANTIFICATION>', '</RESULTS>'), report)

  expect_null(readQuant(report))
})

test_that("the extended shape keeps a compound that appears twice", {
  #paramList was keyed by compound name, so a repeat overwrote the first
  report <- file.path(tempdir(), "repeated_quant_report.xml")
  on.exit(unlink(report), add = TRUE)

  parameter <- function(name, conc) paste0(
    '<PARAMETER name="', name, '" type="quantification">',
    '<VALUE lod="0.3" loq="-" unit="mmol/L" value="', conc, '" valueext="', conc, '"/>',
    '<REFERENCE source="NMR Database" unit="mmol/L" vmax="19" vmin="1"/>',
    '</PARAMETER>')

  writeLines(c('<?xml version="1.0" encoding="utf-8"?>',
               '<RESULTS version="Quant-UR B.1.1.0  Reseach Use Only">',
               '<QUANTIFICATION version="Quant-UR B.1.1.0  Reseach Use Only">',
               parameter("Alanine", 1), parameter("Betaine", 2), parameter("Alanine", 3),
               '</QUANTIFICATION>', '</RESULTS>'), report)

  quant <- readQuant(report)

  expect_equal(nrow(quant$data), 3)
  expect_equal(quant$data$name, c("Alanine", "Betaine", "Alanine"))
  expect_equal(quant$data$conc_v, c("1", "2", "3"))
})

test_that("the extended shape keeps a compound with no REFERENCE", {
  #refMax came from xml_find_all, so no REFERENCE meant a zero row
  #data.frame and the compound vanished from the report
  report <- file.path(tempdir(), "noref_quant_report.xml")
  on.exit(unlink(report), add = TRUE)

  parameter <- function(name, conc, ref) paste0(
    '<PARAMETER name="', name, '" type="quantification">',
    '<VALUE lod="0.3" loq="-" unit="mmol/L" value="', conc, '" valueext="', conc, '"/>',
    if (ref) '<REFERENCE source="NMR Database" unit="mmol/L" vmax="19" vmin="1"/>' else "",
    '</PARAMETER>')

  writeLines(c('<?xml version="1.0" encoding="utf-8"?>',
               '<RESULTS version="Quant-UR B.1.1.0  Reseach Use Only">',
               '<QUANTIFICATION version="Quant-UR B.1.1.0  Reseach Use Only">',
               parameter("Alanine", 1, TRUE), parameter("Betaine", 2, FALSE),
               parameter("Glycine", 3, TRUE),
               '</QUANTIFICATION>', '</RESULTS>'), report)

  quant <- readQuant(report)

  expect_equal(quant$data$name, c("Alanine", "Betaine", "Glycine"))
  expect_equal(quant$data$refMax, c("19", NA, "19"))
  expect_equal(quant$data$conc_v, c("1", "2", "3"))
})
