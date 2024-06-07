library(StreamFind)
library(testthat)

# CoreEngine tests -----

test_that("test empty CoreEngine", {
  expect_equal(class(CoreEngine$new()), c("CoreEngine", "R6"))
})
