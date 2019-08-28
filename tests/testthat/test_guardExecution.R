context("guardExecution")

test_that("guardExecution", {
  expect_type(guardExecution(get('XXX', mode = 'function'), FALSE), 'list')
  expect_type(guardExecution(get('XXX', mode = 'function'), TRUE), 'list')
})
