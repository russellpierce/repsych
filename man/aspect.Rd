\name{aspect}
\alias{aspect}
\title{Solve right-triangles systems involving (monitor) aspect ratios}
\usage{
  aspect(w = NULL, h = NULL, d = NULL, aspect = NULL)
}
\arguments{
  \item{w}{Width}

  \item{h}{Height}

  \item{d}{Diagonal}

  \item{aspect}{Aspect ratio (w/h)}
}
\value{
  numeric The length of the unspecified side
}
\description{
  I designed this to calculate values for monitor aspect
  ratios, but I imagine it is more general. Given any two
  or three values, solve for the missing values.  Note, the
  values are not checked so the principle of GIGO applies.
}
\examples{
pythag(a=3,c=pythag(3,4))
}
\references{
  \url{http://en.wikipedia.org/wiki/Pythagorean_theorem}
}

