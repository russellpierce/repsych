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

<<<<<<< HEAD
  \item{xpoints}{Create visual space}
=======
  \item{xpoints}{Not yet implemented}
>>>>>>> 45812eb6dbf594512f596f108978445bd097e2fa

  \item{...}{other arguments passed to graphics::plot}
}
\value{
  Generates a plot in R graphics
}
\description{
  Draw a catseye plot per idea from Cumming's book
}
\note{
<<<<<<< HEAD
  Check CRAN for 'multicon' on CRAN for Author's newest
  version
}
\examples{
# A Single Group
f <- rnorm(50)
catseye(f, conf=.95, xlab="", ylab="DV", las=1)
catseye(f, conf=.99, xlab="", ylab="DV", las=1, col="light green")

# Two Groups
f2 <- rnorm(100)
g <- rep(1:2, each=50)
catseye(f2, grp=g, xlab="Conditions", ylab="DV", grp.names=c("Control", "Experimental"), las=1)
catseye(f2, grp=g, conf=.8, xlab="Conditions", ylab="DV", grp.names=c("Control", "Experimental"), las=1, col="cyan", main="Two Group Mean Comparison")

# Three Groups
f3 <- c(rnorm(10), rnorm(10, mean=.5), rnorm(10, mean=1, sd=2))
g2 <- rep(1:3, each=10)
catseye(f3, grp=g2, conf=.95, xlab="Conditions", ylab="DV", grp.names=c("Group 1", "Group 2", "Group 3"), las=1, col="cyan", main="Three Group Mean Comparison")

# A 2 x 2 Design
f4 <- rnorm(200)
fac1 <- rep(1:2, each=100)
fac2 <- rep(3:4, 100)
catseye(f4, list(fac1, fac2), xlab="Conditions", ylab="DV", grp.names=c("High/High", "High/Low", "Low/High", "Low/Low"), las=1, col="orange", main="A 2 x 2 Experiment Comparison")
# Using the xpoints argument to create visual space
catseye(f4, list(fac1, fac2), xlab="Conditions", ylab="DV", grp.names=c("High/High", "High/Low", "Low/High", "Low/Low"), xpoints=c(1,2,4,5), las=1, col="orange", main="A 2 x 2 Experiment Comparison")
=======
  Also check CRAN for 'multicon' for Author's newest
  version
}
\examples{
v <- rnorm(50)
catseye(v, conf=.95, xlab="", ylab="DV", las=1)
catseye(v, conf=.99, xlab="", ylab="DV", las=1, col="light green")
>>>>>>> 45812eb6dbf594512f596f108978445bd097e2fa
}
\author{
  Ryne A. Sherman
}

