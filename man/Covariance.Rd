\name{Covariance}
\alias{Covariance}
\title{
	The parametric covariance matrix
}
\description{
	This function parameterizes the decay in covariance of transformed allele frequencies
	between sampled populations/individuals over their pairwise geographic and ecological
	distance.  
}
\usage{
Covariance(a0, aD, aE, a2, GeoDist, EcoDist, delta)
}
\arguments{
  \item{a0}{
	This parameter controls the variance when pairwise distance is zero.  It is the 
	variance of the population-specific transformed allelic deviate (theta) when pairwise 
	distances are zero (i.e. when \eqn{D_{i,j} + E_{i,j} = 0}).
}
  \item{aD}{
	This parameter gives the effect size of geographic distance (\eqn{D_{i,j}}).
}
  \item{aE}{
	This parameter gives the effect size(s) of ecological distance(s) (\eqn{E_{i,j}}).
}
  \item{a2}{
	This parameter controls the shape of the decay in covariance with distance.
}
  \item{GeoDist}{
	Pairwise geographic distance (\eqn{D_{i,j}}).  This may be Euclidean, or, if the 
	geographic scale of sampling merits it, great-circle distance.
}
  \item{EcoDist}{
	Pairwise ecological distance(s) (\eqn{E_{i,j}}), which may be continuous (e.g. - 
	difference in elevation) or binary (same or opposite side of some hypothesized 
	barrier to gene flow).
}
  \item{delta}{
	This gives the size of the "delta shift" on the off-diagonal elements of the 
	parametric covariance matrix, used to ensure its positive-definiteness (even, for 
	example, when there are separate populations sampled at the same 
	geographic/ecological coordinates). This value must be large enough that the 
	covariance matrix is positive-definite, but, if possible, should be smaller than the 
	smallest off-diagonal distance elements, lest it have an undue impact on inference.  
	If the user is concerned that the delta shift is too large relative to the pairwise 
	distance elements in D and E, she should run subsequent analyses, varying the size of 
	delta, to see if it has an impact on model inference.
}
}
\author{
Gideon Bradburd
}
\examples{
#With the HGDP dataset
	data(HGDP.bedassle.data)
	
#Draw random values of the {alpha} parameters from their priors
	alpha0 <- rgamma(1,shape=1,rate=1)
	alphaD <- rexp(1,rate=1)
	alphaE <- matrix(rexp(1,rate=1),nrow=1,ncol=1)
	alpha2 <- runif(1,0.1,2)

#Parameterize the covariance function using the HGDP dataset distances (Geo and Eco)
	example.covariance <- Covariance(a0 = alpha0,aD = alphaD,aE = alphaE,a2 = alpha2,
				GeoDist = HGDP.bedassle.data$GeoDistance,
				EcoDist = list(HGDP.bedassle.data$EcoDistance),
				delta = 0.001)

#Plot the example covariance against geographic distance
	plot(HGDP.bedassle.data$GeoDistance,
		example.covariance,
		pch=19,col=HGDP.bedassle.data$EcoDistance+1,
		main="Covariance in allele frequencies across the Himalayas")
			legend(x="topright",pch=19,col=c(1,2),
				legend=c("same side of Himalayas",
							"opposite sides of Himalayas"))
}
