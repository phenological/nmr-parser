test_that("readExperiment reports one tally per category and nothing escapes", {
  expnos <- c(system.file("HB-COVID0001", "10", package = "nmr.parser"),
              system.file("HB-COVID0001", "11", package = "nmr.parser"))

  #message(cat(...)) put the text on stdout and an empty line on stderr, so
  #none of it could be silenced and every line came with a blank one
  out <- capture.output(suppressMessages(readExperiment(expnos, opts = list(what = c("acqus", "pacs")))))
  expect_equal(length(grep("readExperiment", out)), 0)

  msg <- capture.output(readExperiment(expnos, opts = list(what = c("acqus", "pacs"))),
                        type = "message")
  msg <- msg[nzchar(msg)]

  #one line each, same shape, found over asked
  expect_equal(length(msg), 2)
  expect_match(msg[1], "acqus: 2/2")
  expect_match(msg[2], "pacs: 0/2")
})

test_that("readExperiment names an unknown what", {
  #a typo used to read nothing and say nothing
  expect_warning(readExperiment(system.file("HB-COVID0001", "10", package = "nmr.parser"),
                                opts = list(what = "spectrum")),
                 "unknown what: spectrum")
})

test_that("readExperiment says when an expno folder is not there", {
  expnos <- c(system.file("HB-COVID0001", "10", package = "nmr.parser"), "/no/such/expno")

  msg <- capture.output(readExperiment(expnos, opts = list(what = "title")), type = "message")

  expect_true(any(grepl("1/2 expno folders not found", msg)))
})

test_that("readSpectrum keeps its routine notes quiet on request, never its failures", {
  expno <- system.file("HB-COVID0001", "10", package = "nmr.parser")

  loud <- capture.output(readSpectrum(expno, options = list(eretic = 2, fromTo = c(-0.1, 10))),
                         type = "message")
  quiet <- capture.output(readSpectrum(expno, options = list(eretic = 2, fromTo = c(-0.1, 10),
                                                             verbose = FALSE)),
                          type = "message")

  expect_true(any(grepl("corrected for eretic", loud)))
  expect_equal(length(quiet[nzchar(quiet)]), 0)

  #a failure is reported either way
  failure <- capture.output(readSpectrum(file.path(tempdir(), "nothing_here"),
                                         options = list(verbose = FALSE)),
                            type = "message")
  expect_true(any(grepl("procs file not found", failure)))
})
