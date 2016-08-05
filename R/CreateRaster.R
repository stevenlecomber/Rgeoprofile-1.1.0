CreateRaster <-
function(matrix,levels,transp) {
	tempmat = matrix(1,nrow(matrix),ncol(matrix))
	for (i in 1:(length(levels))) {
		tempmat = tempmat + ((matrix/max(matrix))>levels[i])
		}
	colvec <<- c("transparent",heat.colors(length(levels)-1))
	transp.bit = round(transp*255)
	transp.string = as.hexmode(transp.bit)
	if (transp.bit<16) {transp.string=paste("0",transp.string,sep="")}
	for (i in 2:length(colvec)) {
		colvec[i] = paste(substr(colvec[i],1,7),transp.string,sep="")
		}
	outmat = matrix(colvec[tempmat],nrow=nrow(matrix))
	return(outmat)
	}
