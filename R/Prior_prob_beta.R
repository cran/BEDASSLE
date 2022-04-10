Prior_prob_beta <-
function(beta){
		stats::dgamma(beta,shape=0.001,rate=0.001,log=TRUE)
	}
