
test_that("have extended lipoproteins for single sample", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
    # readLipo("./inst/HB-COVID0001/10/pdata/1/lipo_results.xml")

  expect_false(object = is.null(lipo))
  test <- nmr.parser::extend_lipo(lipo)

  #is calc in the df
  # expect_equal(object = length(grep(pattern = c("calc"), x = colnames(test))) > 0)
  #
  # #is pct in the df
  # expect_true(object = length(grep(pattern = c("pct"), x = colnames(test))) > 0)
  #
  # #is frac in the df
  # expect_true(object = length(grep(pattern = c("frac"), x = colnames(test))) > 0)
  #
  # #total lipids (TL) is now present
  # expect_true(object = length(grep(pattern = c("TL"), x = colnames(test))) > 0)
})

