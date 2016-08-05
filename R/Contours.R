Contours <-
function(xvec,yvec,matrix,levels) {
	flipmat = t(matrix[nrow(matrix):1,])
	conts = contourLines(xvec,yvec,flipmat,levels=levels)
	for (i in 1:length(conts)) {
		lines(conts[[i]]$x,conts[[i]]$y,col="dark grey")
		}
	}
