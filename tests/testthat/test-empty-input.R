#1:length(x) is c(1, 0) when x is empty, so these loops ran twice on nothing
#and started with a subscript that does not exist

test_that("readExperiment accepts an empty list of expnos", {
  for (empty in list(character(0), list())) {
    res <- readExperiment(empty, opts = list(what = c("acqus", "title", "quant")))

    expect_named(res, c("acqus", "title", "quant"))
    for (part in res) expect_equal(nrow(part), 0)
  }
})

test_that("readTitle accepts an empty file", {
  title <- file.path(tempdir(), "empty_title")
  on.exit(unlink(title), add = TRUE)
  file.create(title)

  res <- readTitle(title)

  expect_equal(res$value, "")
})

test_that("scanFolder returns an empty table when there is no acqus to find", {
  #it used to fall through to menu(), which errors non-interactively
  folder <- file.path(tempdir(), "no_acqus_here")
  dir.create(folder, showWarnings = FALSE)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  res <- scanFolder(folder)

  expect_equal(nrow(res), 0)
  expect_named(res, c("file", "EXP", "PULPROG", "USERA2"))
})

test_that("an empty scan feeds into readExperiment without error", {
  folder <- file.path(tempdir(), "no_acqus_either")
  dir.create(folder, showWarnings = FALSE)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  res <- readExperiment(scanFolder(folder)$file, opts = list(what = "acqus"))

  expect_equal(nrow(res$acqus), 0)
})

test_that("scanFolder says how to avoid the prompt instead of failing at it", {
  #menu() returns 0 without asking when there is no prompt to ask at, which
  #was read as a selection and failed with "subscript out of bounds"
  root <- system.file("HB-COVID0001", package = "nmr.parser")

  expect_error(scanFolder(root), "cannot prompt")
  expect_error(scanFolder(root, options = list(EXP = "ignore", PULPROG = "")), "cannot prompt")

  #and the message names the way out
  expect_error(scanFolder(root), "options = list")
})

test_that("scanFolder options still select by pattern", {
  root <- system.file("HB-COVID0001", package = "nmr.parser")

  exact <- scanFolder(root, options = list(EXP = "prof-plasma-noesy", PULPROG = "noesygppr1d"))
  partial <- scanFolder(root, options = list(EXP = "prof-plasma", PULPROG = "noesy"))

  expect_equal(nrow(exact), 1)
  expect_equal(exact$PULPROG, "noesygppr1d")
  expect_equal(partial, exact)
})
