\name{RunMCMC}
\alias{RunMCMC}

\title{
Run MCMC
}
\description{
Runs the MCMC burn-in, sampling and produces diagnostic plots of convergence and autocorrelation. 
}
\usage{
RunMCMC()
}

\details{
This function is the workhorse of the package Rgeoprofile. It takes the model parameters set by \code{ModelParameters} and the prior surface set by \code{CreateMaps} and uses them to run the DPM mixture model. The workflow of this function is as follows:

1) Integration over prior on concentration parameter alpha

2) Run the MCMC burn-in for the specified length of time. The comvergence of chains is shown at this point using the Gelman-Rubin diagnostic statistic, evaluated on the log-likelihood of  the model. Once convergence falls under 1.1 and the minimum number of interations has been reached then the burn-in will terminate.

3) Take MCMC samples as defined by \code{Samples} set in \code{ModelParamters} the plots will show the group assignment of the data points every hundred iterations. 

4) Combine information across all integrated surfaces and produce diagnostic plots. The four plots produced should be: The sigma trace, the posterior distribution of sigma, the auto correlation of the MCMC and the realsied number of sources created by the model.
}
\value{



\item{Sigma }{The posterior draws from sigma}
\item{Group }{The posterior group allocations of the points}
\item{Frequencies  }{The posterior group asigment organised by frequency of points to each source}
\item{Sumdatax }{The sum of all x values for points assigned to each source, required for evaluation of posterior surface}
\item{Sumdatay }{The sum of all y values for points assigned to each source, required for evaluation of posterior surface}

}
\references{
The key description of this method can be found in: 

Stevenson. M.D., and Verity, R. et al. (2013)   Spatial targeting of infectious disease control:identifying multiple, unknown sources.  MEE [In REVIEW]

For an introduction to Markov Chain Monte Carlo sampling see: 

Gelman, A., Carlin, J. B., Stern, H. S., & Rubin, D. B. (2003). Bayesian data analysis. CRC press. 
}
\author{
MD, Stevenson & R, Verity
}

\seealso{
This function makes use of objects created by the functions: \code{ModelParameters}, \code{LoadData} and \code{CreateMaps}
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



}

}