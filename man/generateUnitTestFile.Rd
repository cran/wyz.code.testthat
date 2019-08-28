\name{generateUnitTestFile}
\alias{generateUnitTestFile}
\title{
Generate \code{testthat} Unit Test File
}
\description{
Generate one \code{testthat} unit test file from an instrumented offensive
programming object. One test file per function is created. It holds all the
related tests, organized by evaluation mode.
See \code{\link[wyz.code.offensiveProgramming]{EvaluationMode}} for more information.
}
\usage{
generateUnitTestFile(filename_s_1, content_s,
                     overwrite_b_1 = FALSE,
                     verbose_b_1 = TRUE)
}
\arguments{
  \item{filename_s_1}{ the name of the unit test file to generate }
  \item{content_s}{ the content of the unit test file to generate }
  \item{overwrite_b_1}{ A boolean value, either \code{TRUE} or \code{FALSE} to switch
  on or off the overwritting of the output file.
}
  \item{verbose_b_1}{ A boolean value, either \code{TRUE} or \code{FALSE} to switch
  on or off the output message from this function.
}
}

\value{
Returns the file name. It might be different from the one provided, as \code{testthat}
requires to follow some file naming conventions. They are enforced by this function.
}
\references{
Refer to \code{\link[testthat]{test_file}} from package \code{testthat}.

Refer to \code{\link[wyz.code.offensiveProgramming]{runTestCase}} from package
\code{wyz.code.offensiveProgramming}.
}
\author{
  \packageAuthor{wyz.code.testthat}

  Maintainer: \packageMaintainer{wyz.code.testthat}
}
\note{
This function is provided for convenience. It may ease your own way to generate
\code{testthat} code.

To put this package in action, simply use \code{\link{gautfo}} that offers a
much more straightforward way to get results.
}
\examples{
##---- typical example ----
fn <- file.path(tempdir(), 'myFile')
fn <- generateUnitTestFile(fn, c("# a comment", "x <- 2"))
print(fn)
}
\keyword{ utilities }

