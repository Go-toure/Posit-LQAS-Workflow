# Test processing functions
library(testthat)

test_that("binary conversion works", {
  expect_equal(fast_binary_convert("yes"), 1)
  expect_equal(fast_binary_convert("no"), 0)
  expect_true(is.na(fast_binary_convert("invalid")))
})
