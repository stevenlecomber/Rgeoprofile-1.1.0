reporthitscores <-
function() {

if (length(sources)>0) {
	xdiff = abs(outer(rep(1,nrow(sources)),xvec)-outer(sources[,1],rep(1,gridsize2)))
	ydiff = abs(outer(rep(1,nrow(sources)),yvec)-outer(sources[,2],rep(1,gridsize2)))

	msourcex = mapply(which.min,x=split(xdiff,row(xdiff)))
	msourcey = gridsize2-(mapply(which.min,x=split(ydiff,row(ydiff))))+1

	if (nrow(sources)>1) {
		hitscores = diag(hitscoremat[msourcey,msourcex])
	} else {
		hitscores = hitscoremat[msourcey,msourcex]
	}
	hit_output <<- cbind(sources,hitscores)
	print(hit_output)
}

else { print ("No sources present")}
}
