\name{plot_trace}
\alias{plot_trace}
\title{
	Plot the trace plot for a parameter
}
\description{
	This function plots the parameter value estimated in each sampled generation of the 
	MCMC against the index of that sampled generation.
}
\usage{
plot_trace(parameter, percent.burnin = 0, thinning = 1, 
param.name = deparse(substitute(parameter)))
}
\arguments{
  \item{parameter}{
	The parameter for which the trace plot is being generated.
}
  \item{percent.burnin}{
	The percent of the sampled MCMC generations to be discarded as "burn-in."  If the 
	MCMC is run for 1,000,000 generations, and sampled every 1,000 generations, there 
	will be 1,000 sampled generations.  A \code{percent.burnin} of \code{20} will discard 
	the first 200 sampled parameter values from that sample.
}
  \item{thinning}{
	The multiple by which the sampled MCMC generations are thinned.  A \code{thinning} of
	\code{5} will sample every 5th MCMC generation.
}
  \item{param.name}{
	The name of the parameter for which the trace plot is being displayed.
}
}
\details{
	A trace plot is a basic visual tool for assessing MCMC mixing.  If the chain is 
	mixing well, the trace plot will resemble a "fuzzy caterpillar."  If the trace plot 
	has not plateaued, it is an indication that the chain has not converged on the 
	stationary posterior distribution, and must be run longer.  If the trace plot of a 
	parameter exhibits high autocorrelation, the user may wish to either increase or 
	decrease the scale of the tuning parameter on that parameter, to decrease or increase 
	acceptance rates, respectively.  If the chain appears to be bouncing between areas of 
	"fuzzy caterpillar-dom," it may be an indication of a multi-modal likelihood surface.
}
\author{
Gideon Bradburd
}