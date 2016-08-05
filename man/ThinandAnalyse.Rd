\name{ThinandAnalyse}
\alias{ThinandAnalyse}

\title{
Thin MCMC samples and analyse to construct geoprofile
}
\description{
This function takes the posteriors obtained by the MCMC and thins them to remove autocorrelation as determined by the user. The plots produced by RunMCMC include and auto correlation plot. This can be used to asses the degree of thinning needed. After this step, the remaining samples are used to construct a geoprofile.  
}
\usage{
ThinandAnalyse(thinning = 100)
}

\arguments{
  \item{thinning}{
A value representing the degree of thinning. 100 would keep one value for every 100 samples.

\strong{NOTE:} if you remove too many samples when thinning the number of smaple left to construct the geoprofile will be small, introducing further error. If autocorrelation is large it is recommended that more samples are taken when using RunMCMC. 
}
}
\details{
The steps in this code are the following:

1) The dataset is thinned 
2) The geoprofile is constructed from the remaining posterior samples. 

\strong{NOTE:} This step involves a time intenseive operation (summing multiple matricies together) which is affected by: the number of remaing samples after thinning and the size of the geoprofile being constructed (default gridsize = 300). Increasing the parameter gridsize will result in a higher resolution geoprofile but will make this step take longer, potentially much longer.
}
\value{

\item{Geoprofile }{The final Geoprofile produced by the model, a matrix.}
\item{hitscoremat }{The matrix of hit score values produced by the model, essentialy an ordered search pattern.}

}

\author{
MD, Stevenson
}

\seealso{
\code{RunMCMC}
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


### Thin the samples further due to autocorrelation

ThinandAnalyse(thinning = 500)

}

}
