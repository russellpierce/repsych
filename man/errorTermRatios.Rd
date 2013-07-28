\name{errorTermRatios}
\alias{errorTermRatios}
\title{Calculate the ratio of error terms from an aov object}
\usage{
  errorTermRatios(aov.obj)
}
\arguments{
  \item{aov.obj}{An object of class aov representing an
  ANOVA calculation}
}
\value{
  data.frame
}
\description{
  When the ratio of error terms is low, it may be
  reasonable to aggregate them.  This function lets us
  visual inspect the ratios of the error terms from an aov
  object as a precursor to deciding whether to perform
  error term aggregation.
}
\examples{
data(EBR.Table.18.25)
aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
errorTermRatios(aov.EBR.Table.18.25)
}
\references{
  Rosenthal & Rosnow. (2008). \emph{Essentials of
  Behavioral Research}. Boston: Mc Graw Hill.
}
\seealso{
  Other AggregatingErrorTerms:
  \code{\link{aggregateErrorTerms}},
  \code{\link{effectTerms}}, \code{\link{errorTerms}},
  \code{\link{useAggregateErrorTerms}}
}

