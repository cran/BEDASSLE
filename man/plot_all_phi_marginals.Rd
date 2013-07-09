\name{plot_all_phi_marginals}
\alias{plot_all_phi_marginals}
\title{
	Plot all the marginals for the phi parameters for all populations
}
\description{
	Plots the posterior marginal densities of all phi parameters.  Users may specify
	whether they want a histogram, a density, or both.  For convenience, the \eqn{F_{k}} 
	statistic is presented in place of the phi parameter, as this is the statistic users
	care about. \eqn{F_{k}} is defined as \eqn{\frac{1}{1+phi_{k}}}.
}
\usage{
plot_all_phi_marginals(phi_mat, percent.burnin = 0, thinning = 1, 
population.names = NULL, pop.index= NULL, histogram = TRUE, density = TRUE)
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
	will be 1,000 sampled generations.  A \code{percent.burnin} of \code{20} will discard 
	the first 200 sampled parameter values from that sample.
}
  \item{thinning}{
	The multiple by which the sampled MCMC generations are thinned.  A \code{thinning} of
	\code{5} will sample every 5th MCMC generation.
}
  \item{population.names}{
	A vector of length \code{k}, where \code{k} is the number of populations/individuals 
	(i.e. \code{k = nrow(counts)}), giving the name or identifier of each 
	population/individual included in the analysis.  These will be used to title the
\code{k}
	trace plots of the phi parameters estimated for each population/individual in the
beta-
	binomial model. If \code{population.names} is not provided (i.e.
\code{population.names =
NULL}), 
	a population index number will be used to title the plot.
}
  \item{pop.index}{
	A population index number generated to title a marginal plot if no
\code{population.names}
	is specified.
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