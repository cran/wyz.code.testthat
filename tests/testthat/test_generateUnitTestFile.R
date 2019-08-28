context("generateUnitTestFile")

test_that("generateUnitTestFile", {
  expect_output(generateUnitTestFile(file.path(tempdir(), 'x'), 'x <- 2', FALSE, TRUE))
  expect_type(generateUnitTestFile(file.path(tempdir(), 'test_x'), 'x <- 2'), 'list')
  expect_type(generateUnitTestFile(file.path(tempdir(), 'x.R'), 'x <- 2'), 'list')
  expect_type(generateUnitTestFile(file.path(tempdir(), 'test_x.R'), 'x <- 2'), 'list')
})
