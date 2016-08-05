drootinvgamma <-
function(x,shape,rate,log=F) {
	output = log(2)+shape*log(rate)-lgamma(shape)-(2*shape+1)*log(x)-rate/x^2
	if (log==F) output = exp(output)
	return(output)
}
