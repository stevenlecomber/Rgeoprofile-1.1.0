\name{CreateRaster}

\alias{CreateRaster}

\title{
Creats Raster Images
}

\description{
Creates transpatrent Raster images from the surface to be overlayed on top of the Rgoogle map. 
}

\usage{
CreateRaster(matrix, levels, transp)
}

\arguments{
  \item{matrix}{
The surface matrix to be transformed into a raster image.
}
  \item{levels}{
Graphical parameter to fix the number of levels of the contour plot to be produced as a raster image. Can be any whole number (values from 20-50 tend to look good).
}
  \item{transp}{
Graphical parameter used to set the degree of transparancey of the raster image. Range from 0 (invisbible) to 1 (opaque). 
}
}

\value{
Returns a raster image of that can be overlayed over an existing plot function
}

\author{
M.D.Stevenson
}

