test_that("cleaning removes the characters databases choke on", {
  expect_equal(cleanNames("ddd.aaa"), "ddd-aaa")
  expect_equal(cleanNames("ddd uuu"), "ddd-uuu")
  expect_equal(cleanNames("ddd+aaa"), "dddPaaa")
  expect_equal(cleanNames("ddd*yyy"), "dddTyyy")
  expect_equal(cleanNames("dad*"), "dad-S")
  expect_equal(cleanNames("  spaced   out  "), "spaced-out")
  expect_equal(cleanNames("IVDr04"), "ivdr04")
})

test_that("replicates are suffixed with #", {
  #make.unique() used to be called for its side effect, which R does not have,
  #so duplicates were returned untouched
  expect_equal(cleanNames(c("aa", "aa", "aa")), c("aa", "aa#1", "aa#2"))
  expect_equal(anyDuplicated(cleanNames(rep("sample", 5))), 0)

  #a single name is never suffixed
  expect_equal(cleanNames("aa"), "aa")
})

test_that("names that only collide once cleaned are still made unique", {
  #these three are distinct on input but all clean to "dd-aa", so
  #deduplicating before the substitutions would let duplicates through
  expect_equal(cleanNames(c("dd.aa", "dd_aa", "dd aa")),
               c("dd-aa", "dd-aa#1", "dd-aa#2"))
})
