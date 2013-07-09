\name{plot_phi_trace}
\alias{plot_phi_trace}
\title{
	Plots the trace plot for the phi parameter estimated in a single population
}
\description{
	This function plots the phi parameter value estimated in each sampled generation of 
	the MCMC against the index of that sampled generation.  For convenience, the 
	\eqn{F_{k}} statistic is presented in place of the phi parameter, as this is the 
	statistic users care about.  \eqn{F_{k}} is defined as \eqn{\frac{1}{1+phi_{k}}}.
}
\usage{
plot_phi_trace(phi, percent.burnin = 0, thinning = 1, population.names = NULL, 
pop.index = NULL)
}
\arguments{
  \item{phi}{
	The vector of phi values estimated for a single population from an MCMC run.
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
  \item{population.names}{
	The name of the population/individual for which the marginal density of the phi 
	parameter is being plotted. This will be used to title the marginal plot.  If 
	\code{population.names} is not provided (i.e. \code{population.names = NULL}), a 
	population index number will be used to title the plot.
}
  \item{pop.index}{
	A population index number generated to title a marginal plot if no 
	\code{population.names} is specified.
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