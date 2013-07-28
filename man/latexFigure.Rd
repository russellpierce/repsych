\name{latexFigure}
\alias{latexFigure}
\title{Insert a figure in LaTex}
\usage{
  latexFigure(file, caption = NULL,
    placementSpecifier = "tdp", clearpage = FALSE)
}
\arguments{
  \item{file}{The name of the picture to insert}

  \item{placementSpecifier}{Options for LaTex that go in
  brackets when starting a figure.  Tells LaTex where to
  put the picture. Note that the placement specifier here
  is the latex default, tdp.  When using many images other
  packages, e.g. floats, morefloats may be useful.
  Alternatively, a \\clearpage can clear out the floats.}

  \item{clearpage}{Whether to trigger a \\clearpage after
  the figure.}

  \item{caption}{(optional) The caption for the figure in
  LaTex}
}
\description{
  Insert a figure in LaTex
}

