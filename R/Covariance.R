Covariance <-
function(a0,aD,aE,a2,GeoDist,EcoDist,delta) {
        weighted.dist <- aE[1] * EcoDist[[1]]
        if (length(EcoDist)>1) {
            for (k in 2:length(EcoDist)) {
                weighted.dist <- weighted.dist + aE[k] * EcoDist[[k]]
            }
        }
		covariance <- (1/a0)*exp((-(aD*GeoDist+weighted.dist)^a2)-delta)				#function of geographic distance and ecological distance
		diag(covariance) <- (1/a0)													#delta shift helps avoid problems with non-positive definiteness 
		return(covariance)
	}
