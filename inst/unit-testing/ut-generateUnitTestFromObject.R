library(data.table)
library(wyz.code.offensiveProgramming)
library(wyz.code.testthat)


target_folder <- 'tests/testthat' #"~/tmp/generated"
if (!dir.exists(target_folder)) dir.create(target_folder)

generateTests <- function(sourceFile_s_1, sourcePackage_s_1, object_o_1) {
  g <- gautfo(object_o_1, sourceFile_s_1, sourcePackage_s_1, target_folder)
  print(g)
  g
}

source_package <- 'wyz.code.offensiveProgramming'

source_files <- c(
  'code-samples/full-instrumentation/AdditionTCFIG1.R',
  'code-samples/no-instrumentation/Addition.R',
  'code-samples/fun-defs/good/partial/AdditionFIPartial.R',
  'code-samples/tc-defs/good/partial/AdditionTCPartial.R'
)

sapply(source_files, function(e) {
  source(system.file(e, package = source_package))
})

generateTests(source_files[1], source_package, AdditionTCFIG1())

generateTests(source_files[2], source_package, Addition())

generateTests(source_files[3], source_package, AdditionFIPartial())

generateTests(source_files[4], source_package, AdditionTCPartial())

