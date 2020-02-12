context("opTestthatInformation")

test_that("opTestthatInformation", {
  expect_true(is.data.table(opTestthatInformation()))
})
