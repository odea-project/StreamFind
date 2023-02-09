
test_that("suggested dependencies", {

  expect_true(requireNamespace("streamFindData"))

})

### resources -----------------------------------------------------------------

files <- streamFindData::msFilePaths()

### tests ---------------------------------------------------------------------

sS <- new("streamSet")

test_that("create empty streamSet", {
  expect_s4_class(sS, "streamSet")
  expect_s3_class(setDate(sS), "POSIXt")
})

setTitle(sS) <- "title test"

test_that("setter and getter for the title", {
  expect_equal(setTitle(sS), "title test")
})

test_that("creation of streamSet as msData", {
  expect_s4_class(newStreamSet(files = files[1:2],
                               title = "title test 2",
                               run_parallel = FALSE), "msData")
})
