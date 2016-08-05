\name{CreateMaps}
\alias{CreateMaps}

\title{
Create maps and prior
}
\description{
Creates the base map area for the plotting and calculates the prior, has the option to plot the plot the prior on a google map. Note: needs an internet connection to extract the map information from Google. 
}
\usage{
CreateMaps(PlotPrior = T)
}

\arguments{
  \item{PlotPrior}{
\code{TRUE} of \code{FALSE}. If \code{TRUE} plots a map of the data and prior at a default zoom value. 
}
}
\details{
This is the essential step in the model where the search area is defined around the data and the prior tau is generated. It is at this stage that the prior on soruce emergence (tau) is plotted (if \code{PlotPrior =T}) and the user can examine the map area and prior tau to determine if they are meaningful.
}
\value{

\item{xmin }{a value for the minimum dimensions of the x axis (longitude)}
\item{xmax }{a value for the maximum dimensions of the x axis (longitude)}
\item{ymin }{a value for the minimum dimensions of the y axis (latitude)}
\item{ymax }{a value for the maximumdimensions of the y axis (latitude)}
\item{priorx }{a value for the centre of the prior in the x dimension}
\item{priory }{a value for the centre of the prior in the y dimension}
\item{xvec }{a vector representing the size of the x axis}
\item{yvec }{a vector representing the size of the y axis}
\item{xmat }{a matrix of length \code{xvec}}
\item{ymat }{a matrix of length \code{yvec}}
\item{priormat }{the matrix of prior vaules of tau}


}

\author{
MD, Stevenson
}


\examples{

\dontrun{

### Load in data
### Assign a matrix of lon lat values to mydata

LoadData(Data=mydata)

### Load the model and graphic parameters (using defaults here)

ModelParameters()
GraphicParameters()

### Create the prior and maps

CreateMaps(PlotPrior = T)

### Create prior without the map

CreateMaps(PlotPrior = F)


}

}