\name{winvar}
\alias{winvar}
\title{Calculate the winsorized variance of x
Usage
winvar(x, tr=.2, na.rm=TRUE)
This function is borrowed directly from Rand Wilcox's function and is
kept with this set of functions for capability with a large number
of functions.}
\usage{
  winvar(x, tr = 0.2, na.rm = TRUE)
}
\arguments{
  \item{x}{A numeric vector of which to get the winsorized
  variance}

  \item{tr}{The proportion of scores to winsorize}

  \item{na.rm}{A logical indicating whether missing values
  should be removed prior to calculation.}
}
\description{
  Calculate the winsorized variance of x Usage winvar(x,
  tr=.2, na.rm=TRUE) This function is borrowed directly
  from Rand Wilcox's function and is kept with this set of
  functions for capability with a large number of
  functions.
}
\author{
  Rand Wilcox, borrowed from Ryne Sherman on 2012/09/06
}

