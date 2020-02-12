\name{generateAllUnitTestsFromObject}
\alias{generateAllUnitTestsFromObject}
\alias{gautfo}
\title{
Generate All Unit Tests From Object}
\description{
Generate \code{testthat} code form an instrumented offensive programming object.
}
\usage{
generateAllUnitTestsFromObject(object_o_1, sourceFile_s_1,
                               sourcePackage_s_1, targetFolder_s_1,
                               overwriteFile_b_1 = TRUE, verbose_b_1 = FALSE)
}
\arguments{
  \item{object_o_1}{ the instrumented offensive programming object to consider }
  \item{sourceFile_s_1}{ the source package file related to the offensive
  programming object}
  \item{sourcePackage_s_1}{ the package name that holds the offensive programming object }
  \item{targetFolder_s_1}{ the folder where to write produced \code{testthat} code files }
  \item{overwriteFile_b_1}{ A boolean value, either \code{TRUE} or \code{FALSE} to switch
  on or off the overwritting of the output file. Beware, as it is on by default!
}
  \item{verbose_b_1}{ A boolean value, either \code{TRUE} or \code{FALSE} to switch
  on or off the processing verbosity!
}
}
\details{
Generates automatically, all the \code{testthat} files with compliant \code{thestthat} code,
from the offensive programming object.
}
\value{
A list with two fields named \code{class} and \code{filenames}. Former provides
the class of the analyzed and mined offensive programming object, the latter gives
back all produced \code{testthat} file names.
}
\references{
See \code{\link[wyz.code.offensiveProgramming]{EvaluationMode}} for more information.

Refer to \code{\link[testthat]{test_file}} from package \code{testthat}.

Refer to \code{\link[wyz.code.offensiveProgramming]{runTestCase}} from package
\code{wyz.code.offensiveProgramming}.
}
\author{
  \packageAuthor{wyz.code.testthat}

  Maintainer: \packageMaintainer{wyz.code.testthat}
}
\note{
This is rather easy to use function that implements a quite complex algorithm
based on meta-programmation to generate \code{testthat} code.
}
\examples{
library(data.table)
library(wyz.code.offensiveProgramming)
library(wyz.code.testthat)

source_file <- 'code-samples/both-defs/good/full/AdditionTCFIG1.R'
source_package <- 'wyz.code.offensiveProgramming'
source(system.file(source_file, package = source_package))

object <- AdditionTCFIG1()
g <- gautfo(object, source_file, source_package, tempdir())
print(g)
# $class
# [1] "AdditionTCFIG1"
#
# $filenames
#                                                 filename overwritten
# 1:       /tmp/RtmpKLCrXA/test_AdditionTCFIG1-addDouble.R        TRUE
# 2:      /tmp/RtmpKLCrXA/test_AdditionTCFIG1-addInteger.R        TRUE
# 3:    /tmp/RtmpKLCrXA/test_AdditionTCFIG1-divideByZero.R        TRUE
# 4: /tmp/RtmpKLCrXA/test_AdditionTCFIG1-generateWarning.R        TRUE
# 5:   /tmp/RtmpKLCrXA/test_AdditionTCFIG1-generateError.R        TRUE
# 6:  /tmp/RtmpKLCrXA/test_AdditionTCFIG1-addMultiDouble.R        TRUE
# 7: /tmp/RtmpKLCrXA/test_AdditionTCFIG1-addMultiInteger.R        TRUE
}
\keyword{ utilities }