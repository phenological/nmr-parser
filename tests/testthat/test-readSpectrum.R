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
