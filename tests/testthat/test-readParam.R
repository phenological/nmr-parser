
test_that("reading PULPROG", {
  param <- readParam(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"), "PULPROG")
  expect_equal(param, "noesygppr1d")
})

test_that("reading inexistent file", {
  param <- readParam(system.file("HB-COVID0001", "10", "acq", package = "nmr.parser"), "PULPROG")
  expect_equal(param, NULL)
})

test_that("reading param is empty returns null", {
  param <- readParam(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"), "USERA2")
  expect_equal(param, "")
})

test_that("reading several params at once", {
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("EXP", "PULPROG", "BF1"))

  expect_length(prms, 3)
  expect_equal(prms[[1]], "PROF_PLASMA_NOESY")
  expect_equal(prms[[2]], "noesygppr1d")
  expect_equal(prms[[3]], 600.27)
})

test_that("a param missing among several comes back as NA in place", {
  #the guard on this branch read length(idx > 0), which measures the logical
  #vector rather than the match, and only reached the else by way of
  #length(logical(0)) being 0
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("PULPROG", "NOSUCHPARAM", "BF1"))

  expect_length(prms, 3)
  expect_equal(prms[[1]], "noesygppr1d")
  expect_true(is.na(prms[[2]]))
  expect_equal(prms[[3]], 600.27)

  #the surviving params keep their position, so callers indexing by rank
  #(scanFolder does) are not shifted by the missing one
  expect_equal(names(prms), NULL)
  expect_length(readParam(acqus, c("PULPROG", "NOSUCHPARAM")), 2)
})

test_that("a missing first param does not discard the rest", {
  #the whole file used to be tested on paramName[1] only, so an acqus
  #without an EXP line returned NULL and scanFolder() died indexing prms[[1]]
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("NOSUCHPARAM", "PULPROG"))

  expect_length(prms, 2)
  expect_true(is.na(prms[[1]]))
  expect_equal(prms[[2]], "noesygppr1d")
})

test_that("a param absent from every position still returns NA in place", {
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")
  prms <- readParam(acqus, c("NOSUCHPARAM", "NOSUCHEITHER"))

  expect_length(prms, 2)
  expect_true(all(is.na(unlist(prms))))
})

test_that("a param name is matched against the declaration, not the whole line", {
  #grep(paste0(paramName, "=")) matched the name anywhere in the line, and
  #acqus is ordered alphabetically, so the first hit was often a longer
  #parameter ending in the same letters: LOCSW before SW, DATE before TE,
  #CPDPRG before RG
  acqus <- system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser")

  expect_equal(readParam(acqus, "SW"), 30.0344201255777)   #not LOCSW's 0
  expect_equal(readParam(acqus, "TE"), 310.0016)           #not DATE's timestamp
  expect_equal(readParam(acqus, "RG"), 83.46)              #not CPDPRG, which gave NA

  #the ones that never collided are untouched
  expect_equal(readParam(acqus, "NS"), 32)
  expect_equal(readParam(acqus, "DE"), 6.794558)
  expect_equal(readParam(acqus, "BF1"), 600.27)
  expect_equal(readParam(acqus, "PULPROG"), "noesygppr1d")
})

test_that("a param absent from the file is not satisfied by a longer one", {
  procs <- system.file("HB-COVID0001", "10", "pdata", "1", "procs", package = "nmr.parser")

  #procs has no EXP, but it has MddCEXP, which used to answer for it
  expect_null(readParam(procs, "EXP"))
})

test_that("a value containing = is not cut at the first one", {
  #strsplit(line, "=")[[1]][2] kept only the part before the next =
  acqus <- file.path(tempdir(), "equals_acqus")
  on.exit(unlink(acqus), add = TRUE)
  writeLines(c("##TITLE= Parameter file, TopSpin 3.6.1",
               "##$USERA1= <a=b>",
               "##$PULPROG= <noesygppr1d>",
               "##END="), acqus)

  expect_equal(readParam(acqus, "USERA1"), "a=b")
  expect_equal(readParam(acqus, "PULPROG"), "noesygppr1d")
})

test_that("a name carrying regexp characters is taken literally", {
  acqus <- file.path(tempdir(), "regexp_acqus")
  on.exit(unlink(acqus), add = TRUE)
  writeLines(c("##TITLE= Parameter file, TopSpin 3.6.1",
               "##$A.B= 1",
               "##$AXB= 2",
               "##END="), acqus)

  #"A.B" as a pattern would match AXB too, and AXB comes second
  expect_equal(readParam(acqus, "A.B"), 1)
  expect_equal(readParam(acqus, "AXB"), 2)
})
