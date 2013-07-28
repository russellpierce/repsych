\name{latinSquareCyclic}
\alias{latinSquareCyclic}
\title{Create a cyclic latin square}
\usage{
  latinSquareCyclic(conds)
}
\arguments{
  \item{conds}{A vector of condition IDs or condition
  names}
}
\value{
  (list) Digram balanced latin squares come in pairs when
  there are an odd number of conditions, but only one
  square when there are an even number of conditions. This
  function returns a list with a square (in a matrix) for
  each item.
}
\description{
  This function creates a cyclic latin square
}
\examples{
latinSquareCyclic(1:3)
latinSquareCyclic(LETTERS[1:4])
}
\author{
  Russell S. Pierce \email{Russell.S.Pierce@gmail.com}
}

