\name{catseye}
\alias{catseye}
\title{catseye}
\usage{
  catseye(DV, grp = NULL, plotFUN = mean,
    errFUN = c("ci", "se", "sd"), conf = 0.95,
    xpoints = NULL, grp.names = NULL, tick = FALSE,
    ylim = NULL, col = "gray", ...)
}
\arguments{
  \item{DV}{The variable to be plotted}

  \item{grp}{a vector of group identifiers}

  \item{grp.names}{a character vector of group names}

  \item{plotFUN}{The summary statistics for central
  tendency}

  \item{errFUN}{The function that defines the error for
  plotting (these are internal functions to catseye)}

  \item{conf}{The proportion that represents the desired
  confidence interval}

  \item{tick}{A logicial indicating if you want tick marks
  on the axis}

  \item{ylim}{To limit the bounds of the y axis}

  \item{col}{Set the color}

  \item{xpoints}{Not yet implemented}

  \item{...}{other arguments passed to graphics::plot}
}
\value{
  Generates a plot in R graphics
}
\description{
  Draw a catseye plot per idea from Cumming's book
}
\note{
  Also check CRAN for 'multicon' for Author's newest
  version
}
\examples{
v <- rnorm(50)
catseye(v, conf=.95, xlab="", ylab="DV", las=1)
catseye(v, conf=.99, xlab="", ylab="DV", las=1, col="light green")
}
\author{
  Ryne A. Sherman
}

