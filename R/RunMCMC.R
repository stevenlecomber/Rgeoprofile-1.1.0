RunMCMC <-
function() {

# INTEGRATION --------------------------------------------------

#### Integrate over hyper-prior on alpha (some fancy integration tricks to make this possible). Output in log space, where the ith element of the vector integrated_prob contains the logged integral of (x^i)*gamma(x)/gamma(n+x) over the hyperprior 1/(1+x)^2
cat("Integrating over prior\n")
flush.console()
integrated_prob = rep(0,n)
for (i in 1:n) {
	if (floor(i/10)==(i/10)) {
		cat(paste(i,"of",n,"\n"))
		flush.console()
	}
	temp = rep(0,1001)
	for (j in 2:1001) {
		integrand = function(x) {
			exp((j-501)*log(10) + i*log(x*n) + lgamma(x*n)-lgamma(n+x*n) -2*log(1+x*n))
			}
		temp[j] = integrate(integrand,lower=0,upper=Inf)$value*n
		if (temp[j]<0) temp[j]=0
		temp[j] = log(temp[j]) - (j-501)*log(10)
		if (temp[j]!=-Inf & abs(temp[j]-temp[j-1])<0.0001) {
			integrated_prob[i] = temp[j]
			break()
			}
		}
	}

#### START MCMC BURNIN LOOP  -------------------------------------------------

# Initialise Gibbs sampling objects
mux_burnin = list()
muy_burnin = list()
group_burnin = list()
frequencies_burnin = list()
sumdatax_burnin = list()
sumdatay_burnin = list()
sigma_burnin = list()
convergence = list()
for (chain in 1:chains) {
	mux_burnin[[chain]] = matrix(0,nrow=maxburnin,ncol=n)
		mux_burnin[[chain]][1,] = rnorm(n,sd=100)
	muy_burnin[[chain]] = matrix(0,nrow=maxburnin,ncol=n)
		muy_burnin[[chain]][1,] = rnorm(n,sd=100)
	group_burnin[[chain]] = matrix(1,nrow=maxburnin,ncol=n)
	frequencies_burnin[[chain]] = matrix(0,nrow=maxburnin,ncol=n)
		frequencies_burnin[[chain]][1,1] = n
	sumdatax_burnin[[chain]] = matrix(0,nrow=maxburnin,ncol=n)
		sumdatax_burnin[[chain]][1,1] = sum(datax)
	sumdatay_burnin[[chain]] = matrix(0,nrow=maxburnin,ncol=n)
		sumdatay_burnin[[chain]][1,1] = sum(datay)
	sigma_burnin[[chain]] = rep(1,maxburnin)
	convergence[[chain]] = 1
}

#### Run burnin loop
par(mfrow=c(1,1))
cat("Running burn-in\n")
flush.console()
for (i in 2:maxburnin) {

# Loop through all chains
for (chain in 1:chains) {

	# Update objects with values from last iteration
	mux_burnin[[chain]][i,] = mux_burnin[[chain]][i-1,]
	muy_burnin[[chain]][i,] = muy_burnin[[chain]][i-1,]
	group_burnin[[chain]][i,] = group_burnin[[chain]][i-1,]
	frequencies_burnin[[chain]][i,] = frequencies_burnin[[chain]][i-1,]
	sumdatax_burnin[[chain]][i,] = sumdatax_burnin[[chain]][i-1,]
	sumdatay_burnin[[chain]][i,] = sumdatay_burnin[[chain]][i-1,]
	sigma_burnin[[chain]][i] = sigma_burnin[[chain]][i-1]

	# Perform Gibbs sampling on group allocation
	for (j in 1:n) {
		# Subtract this observation from frequency matrix and other objects
		frequencies_burnin[[chain]][i,group_burnin[[chain]][i,j]] = frequencies_burnin[[chain]][i,group_burnin[[chain]][i,j]] - 1
		sumdatax_burnin[[chain]][i,group_burnin[[chain]][i,j]] = sumdatax_burnin[[chain]][i,group_burnin[[chain]][i,j]] - datax[j]
		sumdatay_burnin[[chain]][i,group_burnin[[chain]][i,j]] = sumdatay_burnin[[chain]][i,group_burnin[[chain]][i,j]] - datay[j]
		# Draw new value of mu with this point removed
		postvar = 1/(frequencies_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2+1/tau^2)
		postmeanx = (sumdatax_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2 + priorx/tau^2)*postvar
		postmeany = (sumdatay_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2 + priory/tau^2)*postvar
		mux_burnin[[chain]][i,group_burnin[[chain]][i,j]] = rnorm(1,mean=postmeanx,sd=sqrt(postvar))
		muy_burnin[[chain]][i,group_burnin[[chain]][i,j]] = rnorm(1,mean=postmeany,sd=sqrt(postvar))
		# Calculate vector of likelihoods for each possible grouping
		probvec = log(frequencies_burnin[[chain]][i,])
		probvec = probvec+dnorm(datax[j],mean=mux_burnin[[chain]][i,],sd=sigma_burnin[[chain]][i],log=T)+dnorm(datay[j],mean=muy_burnin[[chain]][i,],sd=sigma_burnin[[chain]][i],log=T)
		nextgroup = which(frequencies_burnin[[chain]][i,]==0)[1]
		probvec[nextgroup] = integrated_prob[sum(frequencies_burnin[[chain]][i,]>0)+1]-integrated_prob[sum(frequencies_burnin[[chain]][i,]>0)]
		probvec[nextgroup] = probvec[nextgroup] + dnorm(datax[j],mean=priorx,sd=sqrt(sigma_burnin[[chain]][i]^2+tau^2),log=T)+dnorm(datay[j],mean=priory,sd=sqrt(sigma_burnin[[chain]][i]^2+tau^2),log=T)
		probvec = exp(probvec-max(probvec))	#remove underflow
		# Sample from probvec and update relevant objects
		newgroup = sample(n,1,prob=probvec)
		group_burnin[[chain]][i,j] = newgroup
		frequencies_burnin[[chain]][i,newgroup] = frequencies_burnin[[chain]][i,newgroup] + 1
		sumdatax_burnin[[chain]][i,newgroup] = sumdatax_burnin[[chain]][i,newgroup] + datax[j]
		sumdatay_burnin[[chain]][i,newgroup] = sumdatay_burnin[[chain]][i,newgroup] + datay[j]
		if (newgroup==nextgroup) {
			postvar = 1/(frequencies_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2+1/tau^2)
			postmeanx = (sumdatax_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2 + priorx/tau^2)*postvar
			postmeany = (sumdatay_burnin[[chain]][i,group_burnin[[chain]][i,j]]/sigma_burnin[[chain]][i]^2 + priory/tau^2)*postvar
			mux_burnin[[chain]][i,group_burnin[[chain]][i,j]] = rnorm(1,mean=postmeanx,sd=sqrt(postvar))
			muy_burnin[[chain]][i,group_burnin[[chain]][i,j]] = rnorm(1,mean=postmeany,sd=sqrt(postvar))
		}
		}

	# vary sigma conditional on grouping
	sigma_burnin[[chain]][i] = 1/sqrt(rgamma(1,shape=Delta+n,rate=Beta + 0.5*sum((datax - mux_burnin[[chain]][i,group_burnin[[chain]][i,]])^2) + 0.5*sum((datay - muy_burnin[[chain]][i,group_burnin[[chain]][i,]])^2)))
	convergence[[chain]] = mcmc(c(convergence[[chain]],sigma_burnin[[chain]][i]))

	} # End of chain loop

	# Plot convergence
	if (floor(i/10)==(i/10) & i>50) {
		gelman.plot(convergence)
		abline(h=1.1,lty=3)
		if (gelman.diag(convergence)$psrf[2]<1.1 & i>minburnin) break
		}
	
	} # End of burnin loop

#### Plot trace for sigma (standard deviation) across all chains
plot(1,type="n",xlim=c(0,maxburnin),ylim=c(0,0.1),main="sigma trace")
for (chain in 1:chains) {
	points(sigma_burnin[[chain]],ylim=c(0,0.1),pch=20,cex=0.5,col=chain)
}


#### START MCMC MAIN LOOP  ---------------------------------------------------


# Initialise Gibbs sampling objects

mux = matrix(0,nrow=Samples,ncol=n)
	mux[1,] = mux_burnin[[1]][i,]
muy = matrix(0,nrow=Samples,ncol=n)
	muy[1,] = muy_burnin[[1]][i,]
group = matrix(0,nrow=Samples,ncol=n)
	group[1,] = group_burnin[[1]][i,]
frequencies = matrix(0,nrow=Samples,ncol=n)
	frequencies[1,] = frequencies_burnin[[1]][i,]
sumdatax = matrix(0,nrow=Samples,ncol=n)
	sumdatax[1,] = sumdatax_burnin[[1]][i,]
sumdatay = matrix(0,nrow=Samples,ncol=n)
	sumdatay[1,] = sumdatay_burnin[[1]][i,]
sigma = rep(0,Samples)
	sigma[1] = sigma_burnin[[1]][i]
sigma_rate = rep(0,Samples)
	sigma_rate[1] = Beta + 0.5*sum((datax - mux_burnin[[chain]][i,group_burnin[[chain]][i,]])^2) + 0.5*sum((datay - muy_burnin[[chain]][i,group_burnin[[chain]][i,]])^2)

#### Run sample loop
cat("Obtaining samples\n")
flush.console()
for (i in 2:Samples) {

	if (floor(i/10)==(i/10)) {
		cat(paste(i,"of",Samples,"\n"))
		flush.console()
	}

	# Update objects with values from last iteration
	mux[i,] = mux[i-1,]
	muy[i,] = muy[i-1,]
	group[i,] = group[i-1,]
	frequencies[i,] = frequencies[i-1,]
	sumdatax[i,] = sumdatax[i-1,]
	sumdatay[i,] = sumdatay[i-1,]
	sigma[i] = sigma[i-1]

	# Perform Gibbs sampling on group allocation
	for (j in 1:n) {
		# Subtract this observation from frequency matrix and other objects
		frequencies[i,group[i,j]] = frequencies[i,group[i,j]] - 1
		sumdatax[i,group[i,j]] = sumdatax[i,group[i,j]] - datax[j]
		sumdatay[i,group[i,j]] = sumdatay[i,group[i,j]] - datay[j]
		# Draw new value of mu with this point removed
		postvar = 1/(frequencies[i,group[i,j]]/sigma[i]^2+1/tau^2)
		postmeanx = (sumdatax[i,group[i,j]]/sigma[i]^2 + priorx/tau^2)*postvar
		postmeany = (sumdatay[i,group[i,j]]/sigma[i]^2 + priory/tau^2)*postvar
		mux[i,group[i,j]] = rnorm(1,mean=postmeanx,sd=sqrt(postvar))
		muy[i,group[i,j]] = rnorm(1,mean=postmeany,sd=sqrt(postvar))
		# Calculate vector of likelihoods for each possible grouping
		probvec = log(frequencies[i,])
		probvec = probvec+dnorm(datax[j],mean=mux[i,],sd=sigma[i],log=T)+dnorm(datay[j],mean=muy[i,],sd=sigma[i],log=T)
		nextgroup = which(frequencies[i,]==0)[1]
		probvec[nextgroup] = integrated_prob[sum(frequencies[i,]>0)+1]-integrated_prob[sum(frequencies[i,]>0)]
		probvec[nextgroup] = probvec[nextgroup] + dnorm(datax[j],mean=priorx,sd=sqrt(sigma[i]^2+tau^2),log=T)+dnorm(datay[j],mean=priory,sd=sqrt(sigma[i]^2+tau^2),log=T)
		probvec = exp(probvec-max(probvec))	#remove underflow
		# Sample from probvec and update relevant objects
		newgroup = sample(n,1,prob=probvec)
		group[i,j] = newgroup
		frequencies[i,newgroup] = frequencies[i,newgroup] + 1
		sumdatax[i,newgroup] = sumdatax[i,newgroup] + datax[j]
		sumdatay[i,newgroup] = sumdatay[i,newgroup] + datay[j]
		if (newgroup==nextgroup) {
			postvar = 1/(frequencies[i,group[i,j]]/sigma[i]^2+1/tau^2)
			postmeanx = (sumdatax[i,group[i,j]]/sigma[i]^2 + priorx/tau^2)*postvar
			postmeany = (sumdatay[i,group[i,j]]/sigma[i]^2 + priory/tau^2)*postvar
			mux[i,group[i,j]] = rnorm(1,mean=postmeanx,sd=sqrt(postvar))
			muy[i,group[i,j]] = rnorm(1,mean=postmeany,sd=sqrt(postvar))
		}
		}

	# vary sigma conditional on grouping
	sigma_rate[i] = Beta + 0.5*sum((datax - mux[i,group[i,]])^2) + 0.5*sum((datay - muy[i,group[i,]])^2)
	sigma[i] = 1/sqrt(rgamma(1,shape=Delta+n,rate=Beta + 0.5*sum((datax - mux[i,group[i,]])^2) + 0.5*sum((datay - muy[i,group[i,]])^2)))

	# Plot of MCMC grouping for this chain
	if (floor(i/100)==(i/100)) {
		plot(datax,datay,pch=20,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col=MCMCcols2[group[i,]],main=paste("Sample:",i,"Chain:",chain))
		}
	
	} # End of sample loop

#### MCMC DIAGNOSTIC PLOTS  -----------------------------------------------------------

#### Four in one panel
par(mfrow=c(2,2))

# trace plot of sigma (can be removed)
plot(sigma,ylim=c(0,0.1),pch=20,cex=0.5,main="sigma trace")

# distribution of sigma
sdpost = colMeans(mapply(drootinvgamma,x=sdvec,MoreArgs=list(shape=Delta+n,rate=sigma_rate)))
plot(sdvec,sdpost,type="l",xlab="sigma",ylab="posterior density")
lines(sdvec,sdprior,lty=2)

# autocorrelation plot
autocorr.plot(sigma,auto.layout=F,lag.max=Samples)

#Posterior groups histogram
# Removed due to bug...will be up again soon!
#groupnum = rowSums(frequencies>0)
#realised_sources = hist(groupnum,breaks=0:n,plot=F)$intensities
#realised_sources_nonzero = min(which(realised_sources!=0)):max(which(realised_sources!=0))
#barplot(realised_sources[realised_sources_nonzero],names.arg=realised_sources_nonzero,space=0,xlab="Realised Sources",ylab="Probability")

Sigma<<-sigma
Group<<-group
Frequencies<<- frequencies
Sumdatax<<-sumdatax
Sumdatay<<-sumdatay
}
