loglike <-
function(x,sigma,phi,tau) {
  n = length(x)
  val1 = n/sigma^2+1/tau^2
  output = -n/2*log(2*pi)-n*log(sigma)-log(tau)-0.5*log(val1) - 1/(2*val1)*(val1*(sum(x^2)/sigma^2+phi^2/tau^2)-(sum(x)/sigma^2+phi/tau^2)^2)
  return(output)
}
