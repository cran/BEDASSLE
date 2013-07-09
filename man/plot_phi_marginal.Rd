\name{plot_phi_marginal}
\alias{plot_phi_marginal}
\title{
	Plots the marginal for the phi parameter estimated in a single population
}
\description{
	Plots the posterior marginal density of a phi parameter.  Users may specify whether 
	they want a histogram, a density, or both.  For convenience, the \eqn{F_{k}} 
	statistic is presented in place of the phi parameter, as this is the statistic users 
	care about.  \eqn{F_{k}} is defined as \eqn{\frac{1}{1+phi_{k}}}.
}
\usage{
plot_phi_marginal(phi, percent.burnin = 0, thinning = 1, population.names = NULL, 
pop.index = NULL,histogram = TRUE, density = TRUE)
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
  \item{histogram}{
	A switch that controls whether or not the plot contains a histogram of the values 
	estimated for the parameter over the course of the MCMC.  Default is \code{TRUE}.
}
  \item{density}{
	A switch that controls whether or not the plot shows the density of the values 
	estimated for the parameter over the course of the MCMC.  Default is \code{TRUE}.
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