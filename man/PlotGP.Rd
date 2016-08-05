\name{PlotGP}
\alias{PlotGP}
\title{
Plot Geoprofile
}
\description{
Plots the resulting geoprofile overlayed ontop of a google map. The size of the map area plotted can be set by the data, determined by manually clicking on a map using locator or set to certain dimesnions. The geoprofile may be altered by using the graphics paramteres set in \code{GraphicParamters}
}
\usage{
PlotGP(Window = "DEFAULT")
}

\arguments{
  \item{Window}{
Can be set as a list of the format: list(c(minimum x axis, maximum x axis),c(minimum y axis, maximum y axis)). There are two further options: "DEFAULT" which plots a default sized map given the guardrail around the data points or "ZOOM" which produces a map of the default size and then allows the use of \code{locator(2)} clicks to define a zoomed in window, a new map will then be drawn given the size set by \code{locator(2)}.
}
}
\details{
There are three options available in the use of this function: 1) plot a map with exact x and y limits defined using a list (as specificed above) 2) plot a map with a default area defined by a guardrail (default setting 10\% in \code{GraphicParameters}) 3) Plot a map that can be zoomed in with using two clicks of \code{locator}. 

\strong{Note}: when using \code{locator} to set the zoom level, if the x and y axis are not of similar lengths, the resulting Google map can appear distorted.
}
\value{
A plotted map overlayed with a geoprofile
}
\author{
MD, Stevenson
}
\seealso{
\code{GraphicParamters()}
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

### Run the MCMC

RunMCMC()

### Thin and produce the geoprofile

ThinandAnalyse(thinning = 100)



### Finally produce a lovely map and have the geoprofile overlayed

PlotGP(Window = "DEFAULT")

### Change the window to a zoomed in map

PlotGP(Window = "ZOOM")


}

}