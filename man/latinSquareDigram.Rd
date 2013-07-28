\name{latinSquareDigram}
\alias{latinSquareDigram}
\title{Create a digram-balanced latin square}
\usage{
  latinSquareDigram(conds)
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
  This function creates a digram-balanced latin square as
  described in Keppel & Wickens, 2004 pg 384-386. The
  condition orders for subjects are listed by rows, such
  that row one is the order of conditions for subject 1,
  and so on. This method of counter-balancing is preferred
  to so-called cyclic counter-balancing because it balances
  the order of presentation such that conditions occur in
  each position equally and they also balance the order of
  presentation such that each condition occurs before and
  after each other presentation an equal number of times.
  See Keppel & Wickens, 2004 for details.
}
\examples{
latinSquareDigram(1:3)
latinSquareDigram(LETTERS[1:4])
}
\author{
  Russell S. Pierce \email{Russell.S.Pierce@gmail.com}
}
\references{
  Keppel & Wickens, 2004 pp 384-386
}

