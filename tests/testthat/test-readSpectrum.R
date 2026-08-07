test_that("reading missing spectrum", {
  spec <- readSpectrum(system.file("HB-COVID0001", "9", package = "nmr.parser"), options = list("eretic" = 1))
  expect_equal(spec$info[[1]], NULL)
  expect_equal(spec$spec$x[1], NULL)
  expect_equal(spec$spec$y[1], NULL)
  expect_equal(spec$spec$y, NULL)
})

test_that("reading spectrum", {
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"))
  expect_equal(spec$info[[1]], 600.27)
  expect_equal(spec$spec$x[1], -10.3180313)
  expect_equal(spec$spec$y[1], -7.765625)
  expect_equal(length(spec$spec$y), 131072)
  expect_equal(min(spec$spec$y), -4408.0938)
  expect_equal(max(spec$spec$y), 19132144.5)
})

test_that("reading spectrum from 0 to 10", {
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"), options = list("fromTo" = c(-.3, 10)))
  expect_equal(spec$info[[1]], 600.27)
  expect_equal(spec$spec$x[1], -0.3)
  expect_equal(spec$spec$y[1], 30082.988)
  expect_equal(length(spec$spec$y), 44950)
  expect_equal(min(spec$spec$y), 11440.871)
  expect_equal(max(spec$spec$y), 7481293.2)
})

test_that("reading spectrum and apply eretic", {
  eretic <- readEretic(file.path(system.file("HB-COVID0001", "10", package = "nmr.parser"), "QuantFactorSample.xml"))
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                       options = list("fromTo" = c(-.3, 10),
                                      "eretic" = eretic$ereticFactor))
  expect_equal(spec$info[[1]], 600.27)
  expect_equal(spec$spec$x[1], -0.3)
  expect_equal(spec$spec$y[1], 7.8993803)
  expect_equal(length(spec$spec$y), 44950)
  expect_equal(min(spec$spec$y), 3.004216)
  expect_equal(max(spec$spec$y), 1964.48506)
})

test_that("reading spectrum with imaginary part", {
  eretic <- readEretic(file.path(system.file("HB-COVID0001", "10", package = "nmr.parser"), "QuantFactorSample.xml"))
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                       options = list("fromTo" = c(-.3, 10),
                                      "eretic" = eretic$ereticFactor,
                                      "im" = TRUE))
  expect_equal(spec$info[[1]], 600.27)
  expect_equal(spec$spec$x[1], -0.3)
  expect_equal(spec$spec$yi[1], -159.860367)
  expect_equal(length(spec$spec$yi), 44950)
  expect_equal(min(spec$spec$yi), -954.61508)
  expect_equal(max(spec$spec$yi), 1161.0982)
})

test_that("reading spectrum of length 8k", {
  eretic <- readEretic(file.path(system.file("HB-COVID0001", "10", package = "nmr.parser"), "QuantFactorSample.xml"))
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                       options = list("fromTo" = c(-.3, 10),
                                      "eretic" = eretic$ereticFactor,
                                      "im" = TRUE,
                                      "length.out" = 8*1024))
  expect_equal(spec$info[[1]], 600.27)
  expect_equal(spec$spec$x[1], -0.3)
  expect_equal(spec$spec$yi[1], -159.860367)
  expect_equal(length(spec$spec$yi), 8192)
  expect_equal(min(spec$spec$yi), -949.16795)
  expect_equal(max(spec$spec$yi), 1146.23173)
  expect_equal(min(spec$spec$y), 3.2353873)
  expect_equal(max(spec$spec$y), 1932.25414)
})

test_that("reading spectrum and uncalibrate", {
  eretic <- readEretic(file.path(system.file("HB-COVID0001", "10", package = "nmr.parser"), "QuantFactorSample.xml"))
  spec <- readSpectrum(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                       options = list("fromTo" = c(-.3, 10),
                                      "eretic" = eretic$ereticFactor,
                                      "im" = TRUE,
                                      "length.out" = 8*1024,
                                      "uncalibrate" = TRUE))
  expect_equal(spec$info[[1]], 600.270002110706)
  expect_equal(spec$spec$x[1], -0.3)
  expect_equal(spec$spec$yi[1], -159.17903)
  expect_equal(length(spec$spec$yi), 8192)
  expect_equal(min(spec$spec$yi), -933.49887)
  expect_equal(max(spec$spec$yi), 1158.45156)
  expect_equal(min(spec$spec$y), 3.1081375)
  expect_equal(max(spec$spec$y), 1933.97205)
  expect_equal(spec$info[[6]], 1)
})

test_that("a procs missing a parameter is refused, not crashed on", {
  #readParam returns NULL for a parameter it cannot find and c() drops NULL
  #without trace, so the "empty parameter" guard counted nothing and the
  #arithmetic below failed instead, with messages like "argument is of
  #length zero". Inside readExperiment that aborts the whole run over one
  #bad file rather than skipping it
  src <- system.file("HB-COVID0001", "10", package = "nmr.parser")
  procs <- readLines(file.path(src, "pdata", "1", "procs"), warn = FALSE)

  for (param in c("BYTORDP", "SW_p", "OFFSET")) {
    expno <- file.path(tempdir(), paste0("missing_", param))
    dir.create(file.path(expno, "pdata", "1"), recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(expno, recursive = TRUE), add = TRUE)

    file.copy(file.path(src, "acqus"), file.path(expno, "acqus"), overwrite = TRUE)
    file.create(file.path(expno, "pdata", "1", "1r"))
    writeLines(grep(paste0("^##\\$", param, "="), procs, value = TRUE, invert = TRUE),
               file.path(expno, "pdata", "1", "procs"))

    #an error would come back as its message rather than NULL
    result <- tryCatch(readSpectrum(expno), error = function(e) conditionMessage(e))
    expect_null(result)
  }
})

test_that("info always reports an eretic factor", {
  #it was NULL when no correction was applied, and c() drops NULL, so info
  #came back one element short and everything after it moved up an index
  expno <- system.file("HB-COVID0001", "10", package = "nmr.parser")

  plain <- readSpectrum(expno)
  corrected <- readSpectrum(expno, options = list(eretic = 2))

  expect_equal(names(plain$info), names(corrected$info))
  expect_true(is.na(plain$info[["ereticFactor"]]))
  expect_equal(corrected$info[["ereticFactor"]], 2)

  #so a caller indexing info by position is not shifted by the difference
  expect_equal(which(names(plain$info) == "uncalibrated"),
               which(names(corrected$info) == "uncalibrated"))
})
