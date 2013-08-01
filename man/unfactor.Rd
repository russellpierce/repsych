\name{unfactor}
\alias{unfactor}
\title{Unfactor a vector}
\usage{
  unfactor(factors, ignore = ",")
}
\arguments{
  \item{factors}{The vector of factors to be unfactored}

  \item{ignore}{(character) Strings formatted to be used by
  \link{gsub} in the pattern argument to be removed from
  the character vector}

  \item{...}{Additional arguments passed to gsub}
}
\value{
  character or numeric
}
\description{
  Take a factor vector and return a value a vector of
  either type character or numeric by replacing the factors
  with their associated labels.  If all label names are
  numeric, a numeric vector will be returned, otherwise a
  character vector will be returned.
}
\examples{
unfactor(factor(c(3,2,1)))
}

