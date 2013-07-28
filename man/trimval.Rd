\name{trimval}
\alias{trimval}
\title{Trim values
Usage
trimval(x, tr=.2, na.rm=TRUE)}
\usage{
  trimval(x, tr = 0.2, na.rm = TRUE)
}
\arguments{
  \item{x}{A numeric vector to be trimmed}

  \item{tr}{The proportion of values in x to be trimmed}

  \item{na.rm}{A logical indicating whether missing values
  should be removed prior to trimming}
}
\description{
  Trim values Usage trimval(x, tr=.2, na.rm=TRUE)
}
\author{
  borrowed from Ryne Sherman on 2012/09/06.  Modified to
  return NA for removed values by Russell S. Pierce
}

