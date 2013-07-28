\name{aggregateErrorTerms}
\alias{aggregateErrorTerms}
\title{Calculate the aggregated error term from an aov object}
\usage{
  aggregateErrorTerms(aov.obj, terms.to.combine)
}
\arguments{
  \item{aov.obj}{An object of class aov representing an
  ANOVA calculation}

  \item{terms.to.combine}{(vector) A character vector of
  the names of the error terms to combine.  Note that these
  are the names embedded in the aov summary.  It may be
  best to extract them from a use of the
  \code{\link{errorTerms}} function.}
}
\value{
  data.frame
}
\description{
  Calculate the aggregated error term from an aov object
}
\examples{
data(EBR.Table.18.25)
aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
et <- errorTerms(aov.EBR.Table.18.25)
errorTermRatios(aov.EBR.Table.18.25)
et.names <- rownames(et)
terms.to.combine <- et.names[c(2,3,4)]
aggregateErrorTerms(aov.EBR.Table.18.25,terms.to.combine)
}
\references{
  Rosenthal & Rosnow. (2008). \emph{Essentials of
  Behavioral Research}. Boston: Mc Graw Hill.
}
\seealso{
  Other AggregatingErrorTerms: \code{\link{effectTerms}},
  \code{\link{errorTermRatios}}, \code{\link{errorTerms}},
  \code{\link{useAggregateErrorTerms}}
}

