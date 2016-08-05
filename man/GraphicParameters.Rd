\name{GraphicParameters}
\alias{GraphicParameters}

\title{
Loads graphic parameters
}
\description{
Sets the parameters for the Google maps and the plotting area of the surfaces produced by the model. 
}

\usage{
GraphicParameters(Guardrail = 0.05, nring = 20, transp = 0.4, gridsize = 640, gridsize2 = 300, MapType = "roadmap", Location = getwd(), pointcol = "black")
}

\arguments{
  \item{Guardrail}{
Sets the extent of the guardrail around the points. This will fix the size of the profile created and printed to the map. Defaults to 10\% (0.05 in each direction), must be a postive number.
}
  \item{nring}{
The number of rings of the contours and raster image plots produced by PlotGP and CreateMaps, defaults to 20. Generally looks good in the 10-30 region.
}
  \item{transp}{
The transparasncy of the raster image produced by PlotGP and CreateMaps, defaults to 0.4. Must be between 0 and 1, with 0 being completely transparent and 1 being completely opaque .
}
  \item{gridsize}{
The gridszise of the Rgoogle maps, fed to the function GetMap. Google has a maximum gridsize of 640 which is the default, can be betwene 0 and 640.
}
  \item{gridsize2}{
The gridsize of the geoprofile and subequent raster images produced by PlotGP and CreateMaps, defaults to 300. Note, increasing this will increase the computational time, potentially signifcantly. Recommend 150-450, for very high reslution profiles it can be increased, but building the profile in ThinandAnlasye will take substantially more time.
}
  \item{MapType}{
The Google map type of the map produced. Fed to GetMap, defaults to "roadmap" but can be any one of: "roadmap","mobile","satellite", "terrain" or"hybrid".
}
  \item{Location}{
The filepath that Google maps, surfaces and outputs will be saved. Defaults to the working directory. Needs to be working for map generation to take place, check your computer has given R access permissions.
}
  \item{pointcol}{
The colour of the points produced by PlotGP and CreateMaps, defaults to "black".
}
}
\details{
This function loads in and sets all of the graphic parameters used in the resulting map generation, model and plotting. The Location must be set to a useable filepath. The deafults will produce clear plots, nring and MapType can be manually edited in the console at a later point when producing desiered surfaces using PlotGP.
}
\value{

\item{Guardrail }{see description above}
\item{nring }{see description above}
\item{transp}{see description above}
\item{gridsize }{see description above}
\item{gridsize2 }{see description above}
\item{MapType }{see description above}
\item{Location }{see description above}
\item{pointcol }{see description above}

}

\author{
M.D. Stevenson
}

\examples{

\dontrun{

### Load in Data with LoadData

LoadData(Data=mydata)

### Set Graphics Parameters with defaults

GraphicParameters()

### Change maptype to "satellite"

GraphicParameters(MapType = "satellite")

}

}



