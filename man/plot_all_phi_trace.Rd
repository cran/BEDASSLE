\name{plot_all_phi_trace}
\alias{plot_all_phi_trace}
\title{
	Plots all the trace plots for the phi parameters for all populations
}
\description{
	Plots all trace plots for the phi parameters in all populations.  For convenience, 
	the \eqn{F_{k}} statistic is presented in place of the phi parameter, as this is the 
	statistic users care about.  \eqn{F_{k}} is defined as \eqn{\frac{1}{1+phi_{k}}}.
}
\usage{
plot_all_phi_trace(phi_mat, percent.burnin = 0, thinning = 1, population.names = NULL)
}
\arguments{
  \item{phi_mat}{
	The \code{k} by \code{ngen} matrix of phi values estimated for all \code{k} 
	populations/individuals included in the analysis in each of \code{ngen} MCMC 
	generations.
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
	in the beta-binomial model. If \code{population.names} is not provided 
	(i.e. \code{population.names =NULL}), a population index number will be used to 
	title the plot.
}
}
\details{
	A trace plot is a basic visual tool for assessing MCMC mixing.  If the chain is 
	mixing well, the trace plot will resemble a "fuzzy caterpillar."  If the trace plot 
	has not plateaued, it is an indication that the chain has not converged on the 
	stationary posterior distribution, and must be run longer.  If the trace plot of a 
	parameter exhibits high autocorrelation, the user may wish to either increase or 
	decrease the scale of the tuning parameter on that parameter, to decrease or increase 
	acceptance rates, respectively.  If the chain appears to be bouncing between areas 
	of "fuzzy caterpillar-dom," it may be an indication of a multi-modal likelihood 
	surface.
}
\author{
Gideon Bradburd
}