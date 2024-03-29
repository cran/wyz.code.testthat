\name{generateUnitTestFile}
\alias{generateUnitTestFile}
\title{
Generate Testthat Unit Test File
}
\description{
Generate one \bold{\code{testthat}} unit test file from an instrumented offensive
programming object. One test file per function is created. It holds all the
related tests, organized by evaluation mode.
See \bold{\emph{\code{\link[wyz.code.offensiveProgramming]{EvaluationMode}}}}
for more information.
}
\usage{
generateUnitTestFile(filename_s_1, content_s,
                     overwrite_b_1 = FALSE,
                     verbose_b_1 = TRUE)
}
\arguments{
  \item{filename_s_1}{ the name of the unit test file to generate }
  \item{content_s}{ the content of the unit test file to generate }
  \item{overwrite_b_1}{ A boolean value, either \emph{\code{TRUE}} or
  \emph{\code{FALSE}} to switch
  on or off the overwritting of the output file.
}
  \item{verbose_b_1}{ A boolean value, either \emph{\code{TRUE}} or
  \emph{\code{FALSE}} to switch
  on or off the output message from this function.
}
}

\value{
Returns the file name.

It might be different from the one provided, as
\bold{\code{testthat}}
requires to follow some file naming conventions that are enforced by this function.
}
\references{
Refer to \bold{\emph{\code{\link[testthat]{test_file}}}} from package
\bold{\code{testthat}}.

Refer to \bold{\emph{\code{\link[wyz.code.offensiveProgramming]{runTestCase}}}}
from package \bold{\code{wyz.code.offensiveProgramming}}.
}
\author{
  \packageAuthor{wyz.code.testthat}

  Maintainer: \packageMaintainer{wyz.code.testthat}
}
\note{
This function is provided for convenience. It may ease your own way to generate
\bold{\code{testthat}} code.

To put this package in action, simply use \bold{\emph{\code{\link{gautfo}}}}
that offers a much more straightforward way to get results.
}
\examples{
##---- typical example ----
fn <- file.path(tempdir(), 'myFile')
fr <- generateUnitTestFile(fn, c("# a comment", "x <- 2"))
cat(paste(readLines(fr$filename), collapse  = '\n'))
# a comment
# x <- 2}
\keyword{ utilities }

