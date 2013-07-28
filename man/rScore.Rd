\name{rScore}
\alias{rScore}
\title{Calculate an r score}
\usage{
  rScore(data, lambda)
}
\arguments{
  \item{data}{(data.frame) Just the data to L score in the
  wide format}

  \item{lambda}{(numeric) a vector specifying the lambda
  weights to produce the L scores in the same order as the
  columns in data}
}
\description{
  Calculate the correlations (row by row) between sets of
  scores and a set of lambda weights
}

