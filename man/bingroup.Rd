\name{bingroup}
\alias{bingroup}
\title{bingroup}
\usage{
  bingroup(x, weight = NULL, binwidth = NULL,
    origin = NULL, breaks = NULL, range = NULL,
    width = 0.9, drop = FALSE, right = TRUE)
}
\arguments{
  \item{x}{data}

  \item{weight}{Left over code, don't trust it.}

  \item{binwidth}{Bin width to use. Defaults to 1/30 of the
  range of the data}

  \item{origin}{Origin of first bin}

  \item{breaks}{Actual breaks to use. Overrides bin width
  and origin}

  \item{width}{width of bars when used with categorical
  data}

  \item{right}{If TRUE, right-closed, left-open, if FALSE,}

  \item{drop}{If TRUE, remove all bins with zero counts}

  \item{range}{no idea}
}
\description{
  Analogous to ggplot2:::bin
}

