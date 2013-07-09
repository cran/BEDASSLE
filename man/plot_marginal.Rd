\name{plot_marginal}
\alias{plot_marginal}
\title{
	Plots the marginal density of a parameter
}
\description{
	Plots the posterior marginal density of a parameter.  Users may specify whether they 
	want a histogram, a density, or both.
}
\usage{
plot_marginal(parameter, percent.burnin = 0, thinning = 1, histogram = TRUE, 
density = TRUE, population.names = NULL, param.name = deparse(substitute(parameter)))
}
\arguments{
  \item{parameter}{
	The parameter for which the marginal plot is being generated.
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
  \item{histogram}{
	A switch that controls whether or not the plot contains a histogram of the values 
	estimated for the parameter over the course of the MCMC.  Default is \code{TRUE}.
}
  \item{density}{
	A switch that controls whether or not the plot shows the density of the values 
	estimated for the parameter over the course of the MCMC.  Default is \code{TRUE}.
}
  \item{population.names}{
	A vector of length \code{k}, where \code{k} is the number of populations/individuals 
	(i.e. \code{k = nrow(counts)}), giving the name or identifier of each 
	population/individual included in the analysis.  These will be used to title the 
	\code{k} marginal plots of the phi parameters estimated for each 
	population/individual in the beta-binomial model.  If the binomial model is used, 
	\code{population.names} will not be used by this function.
}
  \item{param.name}{
	The name of the parameter for which the trace plot is being displayed.
}
}
\details{
	The marginal plot is another basic visual tool for MCMC diagnosis.  Users should look
	for marginal plots that are "smooth as eggs" (indicating that the chain has been run 
	long enough) and unimodal (indicating a single peak in the likelihood surface).
}
\author{
Gideon Bradburd
}