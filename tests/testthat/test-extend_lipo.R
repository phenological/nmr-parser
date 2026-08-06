
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

test_that("frac divides each subfraction by its own parent class", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  raw <- as.data.frame(t(lipo$data$value))
  colnames(raw) <- lipo$data$id

  res <- nmr.parser::extend_lipo(lipo)$data
  frac <- function(id) res$value[match(paste0(id, "_frac"), res$id)]

  #H* is a share of HD, V* of VL, L* of LD (github issue #2: all three used LD)
  expect_equal(frac("H1PL"), round(100 * raw$H1PL / raw$HDPL, 2))
  expect_equal(frac("H2FC"), round(100 * raw$H2FC / raw$HDFC, 2))
  expect_equal(frac("V1TG"), round(100 * raw$V1TG / raw$VLTG, 2))
  expect_equal(frac("V3CH"), round(100 * raw$V3CH / raw$VLCH, 2))
  expect_equal(frac("L1TG"), round(100 * raw$L1TG / raw$LDTG, 2))

  #same rule for the derived CE columns
  expect_equal(frac("V1CE"), round(100 * (raw$V1CH - raw$V1FC) / (raw$VLCH - raw$VLFC), 2))
})

test_that("pct closes at 100% for every particle", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  res <- nmr.parser::extend_lipo(lipo)$data
  pct <- function(ids) sum(res$value[match(paste0(ids, "_pct"), res$id)])

  #TL is defined as TG + CH + PL, so the composition must sum to 100.
  #round(100 * x, 2) quantises each term to 0.01, so a 4 term sum can drift
  #by up to 0.02; 5e-4 relative (0.05) is the tightest safe bound
  for (p in c("HD", "VL", "ID", "LD")) {
    expect_equal(pct(paste0(p, c("TG", "CH", "PL"))), 100, tolerance = 5e-4)
    #CH is split into CE + FC, so this must close too
    expect_equal(pct(paste0(p, c("TG", "CE", "FC", "PL"))), 100, tolerance = 5e-4)
  }

  #subfractions have no CH column, CE + FC stands in for it
  for (particle in c(paste0("H", 1:4), paste0("V", 1:5), paste0("L", 1:6))) {
    expect_equal(pct(paste0(particle, c("TG", "CE", "FC", "PL"))), 100, tolerance = 5e-4)
  }
})

test_that("frac closes at 100% wherever the denominator is a calculated total", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  res <- nmr.parser::extend_lipo(lipo)$data
  val <- function(ids) sum(res$value[match(ids, res$id)])

  #HDA1, HDA2, LDAB and TBPN are summed from the subfractions in calc,
  #so these fractions are exact by construction
  expect_equal(val(paste0("H", 1:4, "A1_frac")), 100, tolerance = 5e-4)
  expect_equal(val(paste0("H", 1:4, "A2_frac")), 100, tolerance = 5e-4)
  expect_equal(val(paste0("L", 1:6, "AB_frac")), 100, tolerance = 5e-4)

  #particle number is a share of TBPN = VLPN + IDPN + L1..L6PN
  expect_equal(val(c(paste0("L", 1:6, "PN_frac"), "VLPN_pct", "IDPN_pct")), 100,
               tolerance = 5e-4)
})


test_that("reference bounds stay numeric and ordered", {
  lipo <- readLipo(system.file("..", "tests", "testthat", "lipo_results.xml", package = "nmr.parser"))
  res <- nmr.parser::extend_lipo(lipo)$data

  #these used to come back as list columns, since ifelse() on the one row
  #data.frame returned by extend_lipo_value() yields a 1 x n matrix
  expect_true(is.numeric(res$refMax))
  expect_true(is.numeric(res$refMin))
  expect_type(res$refMax, "double")
  expect_type(res$refMin, "double")

  #which is what readLipo() hands over in the first place
  expect_equal(class(res$refMax), class(lipo$data$refMax))

  expect_length(res$refMax, nrow(res))
  expect_true(all(res$refMin <= res$refMax, na.rm = TRUE))

  #the raw parameters are passed through untouched
  expect_equal(res$refMax[match("TPTG", res$id)], lipo$data$refMax[match("TPTG", lipo$data$id)])
  expect_equal(res$refMin[match("TPTG", res$id)], lipo$data$refMin[match("TPTG", lipo$data$id)])
})
