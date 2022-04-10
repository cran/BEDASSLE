BB_Prior_prob_phi <-
function(phi){
		stats::dexp(1/phi,rate=5,log=TRUE)
	}
