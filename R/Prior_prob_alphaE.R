Prior_prob_alphaE <-
function(aE){
		sum( stats::dexp(aE,log=TRUE) )
	}
