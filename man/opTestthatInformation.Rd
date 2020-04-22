\name{opTestthatInformation}
\alias{opTestthatInformation}
\title{Package Functions Information}
\description{list package functions}
\usage{
opTestthatInformation()
}
\value{
A \emph{\code{data.table}} with following columns
\item{name}{the object name}
\item{category}{the category of the object describe by function name. Could be
\emph{\code{CLASS}}, \emph{\code{FUNCTION}} or \emph{\code{DATA}}.}
\item{nature}{either \emph{\code{INTERNAL}} or \emph{\code{EXPORTED}}.}
\item{stratum}{the stratum the object belongs to. Values are \emph{\code{CORE}},
\emph{\code{LAYER_1}}, \emph{\code{LAYER_2}}, \emph{\code{LAYER_3}}.}
\item{phasing}{main usage phase of the object. Values are \emph{\code{DESIGN}},
\emph{\code{BUILD}}, \emph{\code{TEST}}, \emph{\code{RUN}}, \emph{\code{MAINTAIN}},
\emph{\code{EVOLVE}} and \emph{\code{TRANSVERSAL}}.
}
\item{intent}{main global intent of the object. Values are \emph{\code{PARTS_BUILDING}},
\emph{\code{PARTS_ASSEMBLY}}, \emph{\code{QUALITY_CONTROL}}, \emph{\code{FEEDBACK}},
\emph{\code{STATISTICS}},
\emph{\code{CONTENT_GENERATION}} and \emph{\code{UTILITIES}}.}
}
\author{
\packageAuthor{wyz.code.testthat}

Maintainer: \packageMaintainer{wyz.code.testthat}
}
\keyword{documentation}
\concept{documentation generation}
\encoding{UTF-8}
\examples{
opTestthatInformation()

opTestthatInformation()[stratum == 'CORE']
}