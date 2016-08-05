\name{LoadData}
\alias{LoadData}
\title{
Loads in data files
}
\description{
Loads in longitude and latitude data in the form of a matrix or data frame and extracts useful measures to be passed to other functions . Data must be in format of lon then lat. Option to input source data in the same format.
}
\usage{
LoadData(Data = mydata, sources = "NULL")
}
\arguments{
  \item{Data}{
The longitude and latitude point pattern data on which the model will run. \strong{MUST} be inputted as a matrix or data.frame with two columns, longitude then latitude. 
}
  \item{sources}{
Defaults to "NULL" which means that there will be no point sources loaded. If sources are to be loaded they \strong{MUST} be inputted as a matrix or data frame with two columns, longitude then latitude. 
}
}
\details{
This function is the essential starting point of the package Rgeoprofile. This ensures data is loaded in the correct format and then extracts useful information from this data by which the maps and model can be generated.
}
\value{

\item{Data }{A matrix with two columns containing the raw data}
\item{datax }{A vector of londitude values found in Data}
\item{datay }{A vector of latitude values found in Data}
\item{n }{The length of Data}
\item{MCMCcols2}{A sample of colours to be used by the MCMC later}
\item{sources}{If sources is not set to "NULL" the full matrix of sources}

}

\author{
M.D. Stevenson
}


\examples{

### Load in data with no sources
### NOTE YOU NEED TO LOAD IN A DATAFILE CALLED mydata here...

\dontrun{

LoadData(Data = mydata, sources = "NULL")

### Load in data with sources

LoadData(Data = mydata, sources = mysources)

### Check length of data

n

### Check data file

head(Data)

### Check length of datax

length(datax)


}
}
