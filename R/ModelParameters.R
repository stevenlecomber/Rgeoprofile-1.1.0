ModelParameters <-
function(sigma_expectation = 0.02, plotsigma=T,tau = "DEFAULT",Delta= 1,minburnin = 100,maxburnin = 1000,chains = 5,Samples = 10000){
  
  sigma_expectation <<- sigma_expectation
  tau <<-tau 			
  minburnin <<-  minburnin		
  maxburnin <<-  maxburnin			
  chains <<-  chains				
  Samples <<- Samples
  Delta <<- Delta		
  Beta <<- sigma_expectation^2*exp(lgamma(Delta)-lgamma(Delta-0.5))^2
  sdvec <<- seq(0,sigma_expectation*4,length.out=1001)
  sdprior <<- drootinvgamma(sdvec,shape=Delta,rate=Beta)
  
  if (plotsigma==T) {plot(sdvec,sdprior,type="l",xlab="Standard Deviation (Longitude)",ylab="Probability",main="Inverse gamma prior on sigma")
  abline(v=sigma_expectation,lty=2) }
  
}
