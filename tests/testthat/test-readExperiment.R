test_that("reading inexistent folder", {
  expe <- readExperiment(system.file("HB-COVID0001", "101", "acqus", package = "nmr.parser"))
  expect_equal(nrow(expe$acqus), 0)
  expect_length(expe, 9)
})


# test_that("reading inexistent folder", {
#   expe <- readExperiment(list(file.path("/Users/jul/data/covid19_IVDR04_WAFollowUp_241120/10"),
#                               file.path("/Users/jul/data/covid19_IVDR04_WAFollowUp_241120/20")))
#   expect_length(expe, 9)
#   expect_equal(expe$eretic[1]$ereticFactor, "3675.44705508")
#   expect_equal(expe$eretic[2]$ereticFactor, "3693.43456106")
# })

test_that("reading experiment folder", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"))
  expect_length(expe, 9)
  expect_equal(expe$acqus$acqus.TITLE, "Parameter file, TopSpin 3.5 pl 4")
  expect_equal(expe$acqus$acqus.NS, "32")
  expect_equal(expe$procs$procs.PHC0, "-7.223511")
  expect_equal(expe$title$title, "PROF_PLASMA_NOESY Plasma {C:\\IVDrData\\data\\COVID19_Serum-20200701\\nmr} SamTrack {2  A3 - 203}")
  expect_equal(expe$eretic$ereticFactor, "3808.27187511")
  expect_equal(expe$lipo$value.TPTG, 139.84)
  expect_equal(expe$quant$value.Ethanol, "0.000")
  expect_equal(expe$lipo$value.TPTG, 139.84)
  expect_equal(expe$quant$value.Acetone, "0.418")
})

test_that("reading experiment folders", {
  expe<- readExperiment(list(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                             system.file("HB-COVID0001", "9", package = "nmr.parser"),
                             system.file("HB-COVID0001", "11", package = "nmr.parser")))
  expect_length(expe, 9)
  expect_equal(expe$acqus$acqus.TITLE[1], "Parameter file, TopSpin 3.5 pl 4")
  expect_equal(expe$acqus$acqus.NS[2], "32")
  expect_equal(expe$procs$procs.PHC0[1], "-7.223511")

})

test_that("reading experiment folder acqus", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("acqus")))
  expect_length(expe, 1)
  expect_equal(expe$acqus$acqus.TITLE, "Parameter file, TopSpin 3.5 pl 4")
  expect_equal(expe$acqus$acqus.NS, "32")
})

test_that("reading experiment folder procs", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("procs")))
  expect_length(expe, 1)
  expect_equal(expe$procs$procs.PHC0, "-7.223511")
})

test_that("reading experiment folder eretic", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("eretic")))
  expect_length(expe, 1)
  expect_equal(expe$eretic$ereticFactor, "3808.27187511")
})

test_that("reading experiment folder qc", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("qc")))
  expect_length(expe, 1)
  expect_equal(expe$qc$infos[[1]]$name[1], "NMR Experiment Quality Test")
  expect_equal(expe$qc$tests[[1]]$name[1], "LineWidth in Hz")
  expect_equal(expe$qc$tests[[1]]$name[2], "Residual Water Signal in mmol/L")
})

test_that("reading experiment folder spec", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("spec")))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  #the literal is quoted to 7 significant figures, the eretic factor is 3808.27188
  expect_equal(expe$spec$spec[[1]]$info[[5]], 3808.272, tolerance = 1e-6)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 44079)
  expect_length(expe$spec$spec[[1]]$spec$y, 44079)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1,10.0))
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 4358580.9)
})

test_that("reading experiment folder spec", {
  expe <- suppressWarnings(readExperiment(system.file("HB-COVID0001_noEretic",
                                                      "10",
                                                      package = "nmr.parser"),
                                          opts = list(what = c("spec"))))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  expect_equal(expe$spec$spec[[1]]$info[[5]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 44079)
  expect_length(expe$spec$spec[[1]]$spec$y, 44079)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1,10.0))
  # the sum is larger as eretic is not applied
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 16598660907)
})

test_that("reading experiment folder spec", {
  expe <- suppressWarnings(readExperiment(system.file("HB-COVID0001_noEretic",
                                                      "10",
                                                      package = "nmr.parser"),
                                          opts = list(what = c("spec"),
                                                         specOpts = list(uncalibrate = TRUE))))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  expect_length(expe$spec$spec[[1]]$info[[1]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[5]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 44079)
  expect_length(expe$spec$spec[[1]]$spec$y, 44079)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1,10))
  # the sum is larger as eretic is not applied
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 16599536616.8)
})

test_that("reading experiment folder spec testing uncalibrate", {
  expe <- suppressWarnings(readExperiment(system.file("HB-COVID0001_noEretic",
                                                      "10",
                                                      package = "nmr.parser"),
                                          opts = list(what = c("spec"),
                                                         opts = list(uncalibrate = TRUE))))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  expect_length(expe$spec$spec[[1]]$info[[1]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[5]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 44079)
  expect_length(expe$spec$spec[[1]]$spec$y, 44079)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1,10))
  # the sum is larger as eretic is not applied
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 16598660906.7)
})

test_that("reading experiment folder spec testing specOpt", {
  expe <- suppressWarnings(readExperiment(system.file("HB-COVID0001_noEretic",
                                                      "10",
                                                      package = "nmr.parser"),
                                          opts = list(what = c("spec"),
                                                         specOpts = list(uncalibrate = TRUE,
                                                         fromTo = c(-0.1, 10)))))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  expect_length(expe$spec$spec[[1]]$info[[1]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[5]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 44079)
  expect_length(expe$spec$spec[[1]]$spec$y, 44079)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1, 10))
  # the sum is larger as eretic is not applied
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 16599536616.8)
})

test_that("reading experiment folder spec testing specOpt", {
  expe <- suppressWarnings(readExperiment(system.file("HB-COVID0001_noEretic",
                                                      "10",
                                                      package = "nmr.parser"),
                                          opts = list(what = c("spec"),
                                                      specOpts = list(uncalibrate = TRUE,
                                                                      fromTo = c(-0.1, 8),
                                                                      length.out = 10000))))
  expect_length(expe, 1)
  expect_length(expe$spec$spec[[1]]$info, 6)
  expect_length(expe$spec$spec[[1]]$info[[1]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[5]], 1)
  expect_equal(expe$spec$spec[[1]]$info[[1]], 600.270002)
  expect_length(expe$spec$spec[[1]]$spec$x, 10000)
  expect_length(expe$spec$spec[[1]]$spec$y, 10000)
  expect_equal(range(expe$spec$spec[[1]]$spec$x), c(-0.1, 8))
  # the sum is larger as eretic is not applied
  expect_equal(sum(expe$spec$spec[[1]]$spec$y), 4494275678.5)
})

test_that("reading experiment folder lipo", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("lipo")))
  expect_length(expe, 1)
  # expect_equal(expe$lipo$path, "/Users/jul/git/phenological/nmr-parser/inst/HB-COVID0001/10")
  expect_equal(expe$lipo$value.TPTG, 139.84)
})

test_that("reading experiment folder quant", {
  expe<- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"), opts = list(what = c("quant")))
  expect_length(expe, 1)
  # expect_equal(expe$quant$path, "/Users/jul/git/phenological/nmr-parser/inst/HB-COVID0001/10")
  expect_equal(expe$quant$value.Ethanol, "0.000")
})

# test_that("reading experiment folders", {
#   folder <- file.path("~", "Downloads", "BRUKER_600_80", "600")
#   lof <- paste0(folder, "/", dir(folder), "/10")
#   expe<- readExperiment(lof)
#   expect_length(expe, 9)
#   expect_length(expe$acqus$path, 127)
#   expect_equal(nrow(expe$acqus), 127)
#   expect_equal(expe$acqus$path[1], "~/Downloads/BRUKER_600_80/600/32565/10/acqus")
#   expect_equal(expe$qc$infos[[1]]$name[1], "NMR Experiment Quality Test")
#   expect_equal(expe$qc$infos[[1]]$value[1], "passed")
#   expect_equal(expe$qc$tests[[1]]$name[1], "LineWidth in Hz")
#   expect_equal(expe$qc$tests[[1]]$value[1], "1.4")
#   expect_equal(expe$qc$tests[[1]]$refMax[1], "1.5")
#   expect_equal(expe$qc$tests[[2]]$name[1], "LineWidth in Hz")
#   expect_equal(expe$qc$tests[[2]]$value[1], "1.1")
#   expect_equal(expe$qc$tests[[1]]$name[2], "Residual Water Signal in mmol/L")
#   expect_equal(expe$qc$tests[[1]]$value[2], "20.0")
#   expect_equal(expe$lipo$value.TPTG[1], 304.05)
#   expect_equal(expe$quant$value.Acetone[1], "0.089")
#   expect_equal(expe$lipo$value.TPTG[2], 58.86)
#   expect_equal(expe$quant$value.Acetone[2], "0.062")
# })

test_that("reading comet folder", {
  expe<- readExperiment(system.file("EXTERNAL-comet-nmr-urine-R20", "10", package = "nmr.parser"))
  expect_length(expe, 9)
})


test_that("an unknown report version alongside a known one does not stop the run", {
  #there is one report per pdata, but a new version can sit next to the old
  #one. dir() then returns both, and readLipo()/readPacs()/readQc() error on a
  #vector of paths, taking the whole run down. The latest one wins.
  expno <- file.path(tempdir(), "versions", "10")
  pdata <- file.path(expno, "pdata", "1")
  dir.create(pdata, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "versions"), recursive = TRUE), add = TRUE)

  lipoFixture <- system.file("..", "tests", "testthat", "plasma_lipo_report_1_1_0.xml", package = "nmr.parser")
  pacsFixture <- system.file("extdata", "plasma_pacs_report.xml", package = "nmr.parser")
  file.copy(lipoFixture, file.path(pdata, "plasma_lipo_report.xml"))
  file.copy(lipoFixture, file.path(pdata, "plasma_lipo_report_9_9_9.xml"))
  file.copy(pacsFixture, file.path(pdata, "plasma_pacs_report.xml"))
  file.copy(pacsFixture, file.path(pdata, "plasma_pacs_report_9_9_9.xml"))

  expe <- readExperiment(expno, opts = list(what = c("lipo", "pacs")))

  expect_equal(nrow(expe$lipo), 1)
  expect_equal(nrow(expe$pacs), 1)
})

test_that("a trailing slash on the expno still finds the eretic folder", {
  #the last character of the whole path used to be replaced, so ".../11/"
  #became ".../110", no QuantFactorSample.xml was found and the spectrum
  #came back scaled by 1 instead of the eretic factor, behind a warning
  expno <- system.file("HB-COVID0001", "11", package = "nmr.parser")

  bare <- readExperiment(expno, opts = list(what = "spec"))
  slash <- readExperiment(paste0(expno, "/"), opts = list(what = "spec"))

  expect_equal(slash$spec$spec[[1]]$info[["ereticFactor"]],
               bare$spec$spec[[1]]$info[["ereticFactor"]])
  expect_false(slash$spec$spec[[1]]$info[["ereticFactor"]] == 1)
})

test_that("an expno holding only the extended plasma report still yields quant", {
  #plasma_quant_report_ver_1_0.xml was missing from the priority list, so it
  #was found by the dir() scan, fell through every entry, and the sample was
  #silently dropped from res$quant. It is the most common quant report in the
  #archive, 20276 files against 11309 of the plain one.
  expno <- file.path(tempdir(), "ver_only", "10")
  pdata <- file.path(expno, "pdata", "1")
  dir.create(pdata, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "ver_only"), recursive = TRUE), add = TRUE)

  file.copy(system.file("HB-COVID0001", "10", "pdata", "1",
                        "plasma_quant_report_ver_1_0.xml", package = "nmr.parser"),
            file.path(pdata, "plasma_quant_report_ver_1_0.xml"))

  expe <- readExperiment(expno, opts = list(what = "quant"))

  expect_equal(nrow(expe$quant), 1)
  expect_true("value.Ethanol" %in% names(expe$quant))
})

test_that("the plain plasma report still wins when both are present", {
  #the extended report sits below the plain one, so an expno holding both
  #keeps reading what it always read
  expe <- readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                         opts = list(what = "quant"))

  expect_equal(nrow(expe$quant), 1)
  #the plain report carries RELDATA, so sigCorr is populated; the extended
  #shape has none
  expect_equal(as.character(expe$quant$value.Ethanol), "0.000")
})
