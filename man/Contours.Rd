\name{Contours}
\alias{Contours}

\title{
Display contours that work for any zoom level
}
\description{
Displays contours that work for on any zoom level of the maps greated by PlotGP and CreateMaps
}
\usage{
Contours(xvec, yvec, matrix, levels)
}

\arguments{
  \item{xvec}{
A vector of the length of the x axis
}
  \item{yvec}{
A vector of the length of the y axis
}
  \item{matrix}{
A matrix the size and dimensions of the plotting window
}
  \item{levels}{
The number of contour lines to plot}
}

\details{
This function is made use of by \code{PlotGP} and \code{CreateMaps}
}
\value{
Returns a series of contour lines to be overlayed on a graphic device e.g. \code{plot}
}

\author{
M.D. Stevenson
}

