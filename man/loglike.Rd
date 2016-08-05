\name{loglike}
\alias{loglike}

\title{
Loglikelihood calculating function
}
\description{
Calculates the log likelihood of the DPM mixture model as it is running in the MCMC.
}
\usage{
loglike(x, sigma, phi, tau)
}

\arguments{
  \item{x}{
The current sample of the MCMC
}
  \item{sigma}{
The value of the parameter sigma set earlier
}
  \item{phi}{
The value of the parameter phi set earlier
}
  \item{tau}{
The value of the parameter tau set earlier
}
}

\value{

\item{output}{The likelihood of the model}

}

\author{
MD, Stevenson
}

\seealso{
\code{RunMCMC()}
}
