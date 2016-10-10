library(testthat)
setwd("~/Desktop/Fall_2016/Stat159/Stat159Labs/lab7/test-that")

source("functions/range-value.R")
context("Test for range value")
test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(range_value(x), 4)
  expect_length(range_value(x), 1)
  expect_type(range_value(x), 'double')
})


test_that("Test for range including NA", {
  y = c(1,2,3,4,NA)
  expect_length(range_value(y), 1)
  expect_that(range_value(y), equals(NA_real_))
})

test_that("Test for boolean vectors", {
  z = c(T, F, T)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), 'integer')
  expect_that(range_value(z), equals(1L))
})

test_that("Test for incompatible type", {
  w = letters[1:5]
  expect_that(range_value(w), throws_error())
})

test_that("Test for na.rm working", {
  y = c(1,2,3,4,NA)
  expect_length(range_value(y), 1)
  expect_that(range_value(y, T), equals(3))
})

source('functions/missing-values.R')
context("Test for missing value")
test_that("missing_value works as expected", {
  x = c(1,2,3,4,NA)
  expect_length(missing_values(x), 1)
  expect_type(missing_values(x), 'integer')
  expect_gte(missing_values(x), 0)
})