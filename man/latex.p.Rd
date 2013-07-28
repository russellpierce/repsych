\name{latex.p}
\alias{latex.p}
\title{Format a p-value for LaTex}
\usage{
  latex.p(p.val)
}
\arguments{
  \item{p.val}{A numeric p value}
}
\description{
  All ps greater than .01 will be reported exactly to two
  decimal places.  All ps greater than .001 but less than
  .01 will be reported exactly to 3 decimal places.  All
  other ps will be reported at p < .001.
}

