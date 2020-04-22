\name{testthatFactory}
\alias{testthatFactory}
\title{
A factory to produce \code{testthat} code
}
\description{
Produces \code{testthat} code from an instrumented offensive
programming object.
}
\usage{
testthatFactory()
}
\value{
Returns a function that manages dispatch depending on targeted offensive programming
object test case evaluation mode value.
}
\references{
See \bold{\emph{\code{\link[wyz.code.offensiveProgramming]{EvaluationMode}}}}
for more information.

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
\code{testthat} code from an instrumented offensive programming object.

To put this package in action, simply use \code{\link{gautfo}} that offers a
much more straightforward way to get results.
}
\examples{
##---- typical example ----
rv <- testthatFactory()(c(call('isTRUE', TRUE), call('isFALSE', FALSE)), 'correct')
print(rv)
# [[1]]
# expect_true(isTRUE(TRUE))
#
# [[2]]
# expect_true(isFALSE(FALSE))
}
\keyword{ utilities }

