testthatFactory <- function() {

  manageCorrect <- function(call_ca_2) {
    c(
      call('expect_true', call_ca_2[[1]]),
      call('expect_true', call_ca_2[[2]])
    )
  }

  manageErroneous <- function(call_ca_2) {
    c(
      call('expect_true', call_ca_2[[1]]),
      call('expect_false', call_ca_2[[2]])
    )
  }

  manageFailure <- function(call_ca_3) {
    c(
      call('expect_equal', call_ca_3[[1]], call_ca_3[[3]]),
      call('expect_equal', call_ca_3[[2]], 'failure')
    )
  }

  function(call_ca_2m, testTarget_s_1) {
    # if (!is.call(call_ca_1))
    #   abort('parameter call_ca_1 must be a call')

    fn <- switch(testTarget_s_1,
                 'correct' = manageCorrect,
                 'erroneous' = manageErroneous,
                 manageFailure
    )
    fn(call_ca_2m)
  }
}

