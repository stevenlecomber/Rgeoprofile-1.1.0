LoadData <-
function (Data=mydata,sources="NULL") {
  
  Data <<- Data
  datax <<- Data[,1]
  datay <<- Data[,2]
  n <<- length(datax)
  MCMCcols <- colorRampPalette(c('red','green','orange','blue','yellow','gray','black','brown','aquamarine3','cyan','darkmagenta','darkviolet','green4'))
  MCMCcols2 <<- sample(MCMCcols(n))
  cat("data loaded and measures extracted\n")
  print(head(Data))
  
  if (is.data.frame(sources)==T) {
	cat("Source data imported\n")
  	sources <<- sources
	sourcex <<- sources[,1]
  	sourcey <<- sources[,2]
 	print(head(sources))
        } else if (is.matrix(sources)==T) { 
	cat("Source data imported\n")
	print(head(sources))
  	sources <<- sources 
	sourcex <<- sources[,1]
  	sourcey <<- sources[,2]
	} else if (sources== "NULL") {
 	cat("Not importing source data\n") 
  	sources <<- NULL } 
	
  
}
