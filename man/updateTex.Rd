\name{updateTex}
\alias{updateTex}
\title{Update a .tex file from .rnw file if needed}
\usage{
  updateTex(filename)
}
\arguments{
  \item{filename}{character the root of the filename, e.g.
  "writing" when refering to "Writing.Rnw"}
}
\description{
  Check the file dates.  If .Rnw is newer than .Tex, then
  re-weave.
}

