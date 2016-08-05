\name{ModelParameters}
\alias{ModelParameters}

\title{
Loads model paramters
}
\description{
Sets the parameters for the model and the MCMC. Also has the option to plot out the inverse gamma prior on sigma.
}
\usage{
ModelParameters(sigma_expectation = 0.02, plotsigma = T, tau = "DEFAULT", Delta = 1, minburnin = 100, maxburnin = 1000, chains = 5, Samples = 10000)
}

\arguments{
  \item{sigma_expectation}{
The expecatation of the inverse gamma prior on the paramter sigma. What you expect to be a reasonable value for sigma. If possible use an informative value for this prior. Units in decimal degrees.
}
  \item{plotsigma}{
If \code{TRUE} plots the inverse gamma prior on sigma.
}
  \item{tau}{
Set as \code{"DEFAULT"} meaning that the paramter tau (the prior on source emergence) is calculated from the area of the data.
}
  \item{Delta}{
A parameter in the inverse gamma prior on sigma, defaults to 1. Must be larger than 0.5.
}
  \item{minburnin}{
The minimum burn-in for the MCMC before it can begin sampling, defaults to 100. It is recommended to be 100-500.
}
  \item{maxburnin}{
The maximum burn-in for the MCMC before it can begin sampling, defaults to 1000. It is recommended to be 1000-5000.}
  \item{chains}{
Number of chains run in the MCMC, defaults to 5. Reccomend 5-10. 
}
  \item{Samples}{
Number of sample to be taken from the MCMC, this will depend upon convergence and autocorrelation. Defaults to 10,000, reccomend 10,000 to 100,000.
}
}
\details{
This function sets the important model parameters as well as the values to be used by the MCMC. The most important value is the expectation on sigma. This would ideally be set as an informative prior on sigma, using knowledge of the organism in questions dispersal and movement. The units of this value are in lattitude decimal degrees. Use \code{plotprior=T} to examine the prior on the dispersal paramter sigma to make sure it is sensible.
}
\value{

\item{sigma_expectation }{The expecatation of the inverse gamma prior on the paramter sigma, units in decimal degrees }
\item{tau }{Set as "DEFAULT"}
\item{minburnin }{The minimum burn-in for the MCMC before it can begin sampling, defaults to 100 }
\item{maxburnin }{The maximum burn-in for the MCMC before it can begin sampling, defaults to 1000 }
\item{chains }{Number of chains run in the MCMC, defaults to 5 }
\item{Samples }{Number of sample to be taken from the MCMC, defaults to 10,000 }
\item{Delta }{A parameter in the inverse gamma prior on sigma, defaults to 1}
\item{Beta }{Other parameter of inverge gamma on sigma, calculated from Delta and the sigma_expecatation }
\item{sdvec }{A vector standard deviation values from the inverse gamma prior on sigma}
\item{sdprior }{The standdard deviation of the prior value of sigma used to generate the figure of the prior distribution of sigma. }

}

\author{
M.D.Stevenson
}

\examples{

\dontrun{

#### Load in data first

LoadData(data=mydata)

### Set up default model parameters and examine the prior

ModelParameters()

### Change the parameter sigma to 0.05 and Delta to 0.7

ModelParameters(sigma_expectation = 0.05, Delta = 0.7)

### Decrease burn-in and number of samples taken (to speed up the running of the MCMC)

ModelParameters(minburnin = 100, maxburnin = 1000, Samples = 10000)
}
}