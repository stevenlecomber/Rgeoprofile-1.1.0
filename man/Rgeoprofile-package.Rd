\name{Rgeoprofile-package}
\alias{Rgeoprofile-package}
\alias{Rgeoprofile}
\docType{package}
\title{
Geographic Profiling in R

}
\description{
Create Geographic Profiles using point pattern data. The methods used are based on the Dirichlet Proccess Mixture Model. The package: sets the graphical parameters, uses Rgooglemaps to map the data points, creates a Bayesian mixture model and fits parameters with a MCMC, renders the posterior surface and diagnostic plots and finally outputs source information and statistics.
}
\details{
\tabular{ll}{
Package: \tab Rgeoprofile\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-06-11\cr
License: \tab GPL-2 | GPL-3\cr
}
This package runs the DPM mixture model. The package is composed of 8 core functions and 4 accessory functions. The core functions load in the data, set parameters of the model, generates the prior, runs the MCMC, produces maps and saves the results. They should be run in order:

1) \code{LoadData()}: Loads in data, extracts key information, must bre run first.

2) \code{ModelParamters()}: Sets model parameters, option to plot prior on parameter sigma

3) \code{GraphicParamters()}: Sets graphic parameters of maps and plots

4) \code{CreateMaps()}: Creates the prior surface on source location tau and plots the data on a Google map

5) \code{RunMCMC()}: Runs the bulk of the analysis using MCMC (a time intesive step)

6) \code{ThinandAnalsye()}: Thins the posterior samples and construcst the geoprofile (a time intesive step) 

7) \code{PlotGP()}: Plots the data and overlays the geoprofile onto a Google map

8) \code{reporthitscores()}: OPTIONAL if source data is imported, calculates the hit scores of potential sources.

See indvidual descriptions for the use of these functions.

}
\author{
M.D. Stevenson and R. Verity

Maintainer:  <m.stevenson@qmul.ac.uk>

}
\references{
Stevenson. M.D., and Verity, R. et al. (2013)   Spatial targeting of infectious disease control:identifying multiple, unknown sources.  MEE [In REVIEW]
}

\keyword{ package }

\seealso{
Geographic Profiling in Biology \cr
http://evolve.sbcs.qmul.ac.uk/lecomber/sample-page/geographic-profiling/
}
\examples{
\dontrun{

### The package follows 8 steps using 8 seperate functions

### 1) Load in data
### Assign a matrix of lon lat values to mydata

LoadData(Data=mydata)

### 2) + 3) Load the model and graphic parameters (using defaults here)

ModelParameters()
GraphicParameters()

### 4) Create the prior and maps

CreateMaps(PlotPrior = T)

### 5) Run the MCMC

RunMCMC()

###  6) Thin and produce the geoprofile

ThinandAnalyse(thinning = 100)


### 7) Produce a lovely map and have the geoprofile overlayed

PlotGP(Window = "DEFAULT")

### 8) Report the hit scores of sources, only if sources have been loaded in

reporthitscores()

}



}
