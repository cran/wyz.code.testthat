context("reifyObject")

Bu_S3 <- function(x_l = list(l = letters, d = 0:9)) {
  value <- x_l
  attr(value, 'class') <- 'Bu_S3'
  value
}

Person_S4 <- setClass("Person_S4",
                      slots = c(
                        name = "character",
                        age = "numeric"
                      )
)

setMethod("show", "Person_S4", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Age:  ", object@age, "\n",
      sep = ""
  )
})

setGeneric("name", function(o_) standardGeneric("name"))
setMethod("name", "Person_S4", function(o_) o_@name)

Person_RC <- setRefClass("Person_RC",
                         fields = list(name = "character",
                                       age = "integer"),
                         methods = list(
                           setName = function(aname) {
                             name <<- aname
                           },
                           setAge = function(anAge) {
                             age <<- anAge
                           }
                         )
)

Accumulator_R6 <- R6::R6Class("Accumulator_R6", list(
  sum = 0,

  add = function(x = 1) {
    self$sum <- self$sum + x
    self$sum
  })
)

MyEnv <- function(x_i, y_i = 1L) {
  self <- environment()
  class(self) <- append('MyEnv', class(self))

  self
}

# Why does that fail? Testthat context is not seing previously defined
# functions
obj <- list(
  guardExecution(reifyObject(MyEnv(1:3), '', '')),
  guardExecution(reifyObject(Bu_S3(), '', '')),
  guardExecution(reifyObject(new('Person_RC', name = 'neonira'), '', '')),
  guardExecution(reifyObject(new('Person_S4', name = 'neonira'), '', '')),
  guardExecution(reifyObject(Accumulator_R6$new(), '', ''))
)

#print(obj)

test_that("reifyObject", {

  expect_true(is.na(reifyObject(new.env(), '', '')))
  expect_error(reifyObject(Bu_S3(), '', ''))
  expect_error(reifyObject(new('Person_RC', name = 'neonira'), '', ''))
  expect_error(reifyObject(new('Person_S4', name = 'neonira'), '', ''))
  expect_error(reifyObject(Accumulator_R6$new(), '', ''))
  expect_error(reifyObject(MyEnv(1:3), '', ''))
})
