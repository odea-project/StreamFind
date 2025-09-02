library(StreamFind)
library(testthat)

# Engine tests -----

test_that("test empty Engine", {
  expect_equal(class(Engine$new()), c("Engine", "R6"))
})
