test_that("reading inexistent file", {
  param <- readParams(system.file("HB-COVID0001", "10", "acq", package = "nmr.parser"))
  expect_equal(param, NULL)
})

test_that("checking dimensions", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 1128)
  expect_equal(ncol(param), 3)
})

test_that("checking dimensions", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20", "10", "acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 902)
  expect_equal(ncol(param), 3)
})

test_that("checking PULPROG", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "PULPROG"], "noesygppr1d")
})

test_that("checking USERA2", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "USERA2"], "")
})

test_that("checking D1", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "D_1"], "4")
})

test_that("checking NPOINTS", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "NPOINTS"], "13 modification sequence number")
})

test_that("checking instrument", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrument"], "nmrsu-rmn601")
})

test_that("checking date", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentDate"], "2020-07-01")
})

test_that("checking time", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTime"], "16:55:49.592")
})

test_that("checking timezone", {
  param <- readParams(system.file("HB-COVID0001", "10", "acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTimeZone"], "+0200")
})

test_that("checking timezone xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTimeZone"], "BST (UT+1h)")
})

test_that("checking time xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentTime"], "12:08:08")
})

test_that("checking date xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrumentDate"], "Wed Jun 2 2004")
})

test_that("checking instrument xwinnmr", {
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))
  expect_equal(param$value[param$name == "instrument"], "comet-bc-jkn-17-sk-med-ic-ac-uk")
})

test_that("handling amix files", {
  param <- readParams(system.file("hydroxyproline", "1dno-d2o-7-g", "acqus", package = "nmr.parser"))
  expect_equal(param, NULL)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20-10-acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 902)
  expect_equal(ncol(param), 3)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20", "10", "acqus", package = "nmr.parser"))
  expect_equal(nrow(param), 902)
  expect_equal(ncol(param), 3)
})

test_that("test comet file", {
  param <- readParams(system.file("EXTERNAL-comet-nmr-urine-R20", "10", "pdata" , "1", "procs", package = "nmr.parser"))
  expect_equal(param, NULL)
})

test_that("the last line of the file is read", {
  #the loop ran while counter < length(txt), so the final parameter was
  #dropped whenever the file did not end on ##END=
  param <- readParams(system.file("xwinnmr_acqus", package = "nmr.parser"))

  expect_equal(param$name[nrow(param)], "SUBNAM7")
  expect_equal(nrow(param), 850)
})

test_that("a file truncated on a vector header does not overrun", {
  #a vector parameter reads its values from the next line, which is not
  #there when the file ends on the header
  acqus <- file.path(tempdir(), "truncated_acqus")
  on.exit(unlink(acqus), add = TRUE)
  writeLines(c("##TITLE= Parameter file, TopSpin 3.6.1",
               "##$PULPROG= <noesygppr1d>",
               "##$AMP= (0..31)"), acqus)

  param <- readParams(acqus)

  expect_equal(param$name, c("TITLE", "PULPROG"))
})

test_that("only the < > delimiters are stripped from a value", {
  #[<->] is a range from < to >, so it swallowed = as well
  acqus <- file.path(tempdir(), "delimiters_acqus")
  on.exit(unlink(acqus), add = TRUE)
  writeLines(c("##TITLE= Parameter file, TopSpin 3.6.1",
               "##$USERA1= <a=b>",
               "##$USERA2= <plain>",
               "##END="), acqus)

  param <- readParams(acqus)

  expect_equal(param$value[param$name == "USERA1"], "a=b")
  expect_equal(param$value[param$name == "USERA2"], "plain")
})
