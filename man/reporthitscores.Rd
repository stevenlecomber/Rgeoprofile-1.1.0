\name{reporthitscores}
\alias{reporthitscores}

\title{
Report Hit Scores
}
\description{
Calculates hit score values for sources given a previously generated geoprofile. Requires both loaded in sources (i.e. \code{sources} is not set to \code{"NULL"} in \code{LoadData}) and an object \code{hitscoremat} the result of \code{ThinandAnalyse}.
}
\usage{
reporthitscores()
}


\value{

\item{hit_output }{A data frame of three columns and a number of rows equal to the length of sources. The rows are: the longitude of the soruces, the latitude of the sources and the hit score of the source}

}
\references{
For a discussion of hit score values and their use in geographic profiling in Biology see: Stevenson et al. (2012) Geographic Profiling as a novel spatial tool for targeting the control of invasive species. Ecography. Where this method of assessing search strategies is defined.  
}
\author{
MD, Stevenson
}

\seealso{

\code{LoadData}, \code{ThinandAnalyse}
}
