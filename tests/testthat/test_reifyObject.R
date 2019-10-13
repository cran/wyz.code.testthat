context("reifyObject")

source(file.path(system.file(package = "wyz.code.offensiveProgramming"),
                 'code-samples', 'classes', 'sample-classes.R'))


obj <- list( MyEnv(),
             Bu_S3(),
             new('Person_RC', name = 'neonira'),
             new('Person_S4', name = 'neonira'),
             Wyx(1:7),
             Accumulator_R6$new()
)

#print(obj)

robj <- lapply(obj, function(e) tryCatch(wyz.code.testthat:::reifyObject(e, '', ''),
                                         error = function(e) e))

#print(obj)

test_that("reifyObject", {

  expect_true(is.na(reifyObject(new.env(), '', '')))
  sapply(seq_len(length(robj)), function(k) {
    ock <- wyz.code.offensiveProgramming::getObjectClassNames(obj[[k]])$classname
    b <- grepl(ock, as.character(robj[[k]]$to_reify))[1]
    #cat('\n', k, ock, 'robj', as.character(robj[[k]]$to_reify), 'result', b, '\n')
    expect_true(b)
  })
})
