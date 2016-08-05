ThinandAnalyse <-
function(thinning=100) {

thinning <<- thinning


sigma_thin = Sigma[seq(1,Samples,thinning)]
group_thin = Group[seq(1,Samples,thinning),]
frequencies_thin = Frequencies[seq(1,Samples,thinning),]
sumdatax_thin = Sumdatax[seq(1,Samples,thinning),]
sumdatay_thin = Sumdatay[seq(1,Samples,thinning),]

#### CONSTRUCT GEOPROFILE  -----------------------------------------------------------

#### Construct geoprofile by averaging over conditional posterior distributions
cat("Constructing geoprofile\n")
flush.console()

postvar = 1/(frequencies_thin/outer(sigma_thin,rep(1,n))^2+1/tau^2)
postmeanx = (sumdatax_thin/outer(sigma_thin,rep(1,n))^2 + priorx/tau^2)*postvar
postmeany = (sumdatay_thin/outer(sigma_thin,rep(1,n))^2 + priory/tau^2)*postvar
Geoprofile = matrix(0,nrow=gridsize2,ncol=gridsize2)
for (i in 1:gridsize2) {
	if (floor(i/5)==(i/5)) {
		cat(paste(i,"of",gridsize2,"\n"))
		flush.console()
	}
	M1 = outer(dnorm(yvec[gridsize2+1-i],mean=postmeany[frequencies_thin>0],sd=sqrt(postvar[frequencies_thin>0])),rep(1,gridsize2))
	M2 = mapply(dnorm,x=xvec,MoreArgs=list(mean=postmeanx[frequencies_thin>0],sd=sqrt(postvar[frequencies_thin>0])))
	Geoprofile[i,] = colSums(M1*M2)
}
Geoprofile <<- Geoprofile

#### Ordermat is the final matrix of hit scores

ordermat = matrix(0,gridsize2,gridsize2)
profile_order = order(Geoprofile)
for (i in 1:gridsize2^2) {
	ordermat[profile_order[i]] = i
	}
hitscoremat <<- 1-ordermat/gridsize2^2

#### Create coassignment matrix for threshold grouping

coassign = matrix(0,n,n)
for (i in 1:n) {
	for (j in 1:n) {
		coassign[i,j] = mean(group_thin[,i]==group_thin[,j])
		}
	}
thresholdgroups = rep(8,n)
for (i in 1:n) {
	z = (coassign[i,]>(0.9))
	if (sum(z)>1) {
		thresholdgroups[z]=i
		}
	}

}
