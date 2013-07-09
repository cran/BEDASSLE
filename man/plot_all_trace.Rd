\name{plot_all_trace}
\alias{plot_all_trace}
\title{
	Plots all the trace plots for all parameters
}
\description{
	This function plots the parameter value estimated in each sampled generation of the 
	MCMC against the index of that sampled generation for each parameter in the model.
}
\usage{
plot_all_trace(MCMC.output, percent.burnin = 0, thinning = 1, population.names = NULL)
}
\arguments{
  \item{MCMC.output}{
	The standard MCMC output file generated from a BEDASSLE run.
}
  \item{percent.burnin}{
	The percent of the sampled MCMC generations to be discarded as "burn-in."  If the 
	MCMC is run for 1,000,000 generations, and sampled every 1,000 generations, there 
	will be 1,000 sampled generations.  A \code{percent.burnin} of \code{20} will 
	discard the first 200 sampled parameter values from that sample.
}
  \item{thinning}{
	The multiple by which the sampled MCMC generations are thinned.  A \code{thinning} of
	\code{5} will sample every 5th MCMC generation.
}
  \item{population.names}{
	A vector of length \code{k}, where \code{k} is the number of populations/individuals 
	(i.e. \code{k = nrow(counts)}), giving the name or identifier of each 
	population/individual included in the analysis.  These will be used to title the 
	\code{k} trace plots of the phi parameters estimated for each population/individual 
	in the beta-binomial model.  If the binomial model is used, \code{population.names} 
	will not be used by this function.
}
}
\details{
	A trace plot is a basic visual tool for assessing MCMC mixing.  If the chain is mixing
	well, the trace plot will resemble a "fuzzy caterpillar."  If the trace plot has not 
	plateaued, it is an indication that the chain has not converged on the stationary 
	posterior distribution, and must be run longer.  If the trace plot of a parameter 
	exhibits high autocorrelation, the user may wish to either increase or decrease the 
	scale of the tuning parameter on that parameter, to decrease or increase acceptance 
	rates, respectively.  If the chain appears to be bouncing between areas of "fuzzy
	caterpillar-dom," it may be an indication of a multi-modal likelihood surface.
}
\author{
Gideon Bradburd
}