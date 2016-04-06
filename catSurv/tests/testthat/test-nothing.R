library(testthat)

context("Testing absolutely nothing")

test_that("addition does addition",{
  expect_that(3+4, equals(7))
})

test_that("a failure is a failure", {
  expect_that(3+4, equals(8))
})