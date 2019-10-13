context("gautfo")

generateTests <- function(sourceFile_s_1, sourcePackage_s_1, object_o_1) {
  target_folder <- file.path(tempdir(), 'tests/testthat')
  if (!dir.exists(target_folder)) dir.create(target_folder, recursive = TRUE)
  gautfo(object_o_1, sourceFile_s_1, sourcePackage_s_1, target_folder)
}

source_package <- 'wyz.code.offensiveProgramming'

source_files <- c(
  'code-samples/both-defs/good/full/AdditionTCFIG1.R',
  'code-samples/no-defs/Addition.R',
  'code-samples/frt-defs/good/partial/AdditionFIPartial.R',
  'code-samples/tcd-defs/good/partial/AdditionTCPartial.R'
)

sapply(source_files, function(e) {
  source(system.file(e, package = source_package))
})

result <- list(
  generateTests(source_files[1], source_package, AdditionTCFIG1()),
  generateTests(source_files[2], source_package, Addition()),
  generateTests(source_files[3], source_package, AdditionFIPartial()),
  generateTests(source_files[4], source_package, AdditionTCPartial())
)


test_that("gautfo", {
  #  not existing target folder
  expect_error(gautfo(AdditionTCFIG1(), source_files[1], source_package, '/XXX'))

  # rightly offensive programming instrumented object
  expect_type(result[[1]], 'list')
  expect_true(is.data.table(result[[1]]$filenames))

  # incompletely offensive programming instrumented object
  expect_type(result[[2]], 'character')
  expect_type(result[[3]], 'character')
  expect_type(result[[4]], 'character')
})
