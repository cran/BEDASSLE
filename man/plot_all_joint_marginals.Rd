\name{plot_all_joint_marginals}
\alias{plot_all_joint_marginals}
\title{
	Plots the joint marginals for all parameter pairs
}
\description{
	For each sampled MCMC generation, the values estimated for a pair of parameters are 
	logged and plotted against one another.  Points are color coded by when in the 
	analysis they were sampled, so that users can visually assess mixing.  A joint 
	marginal plot is generated for all combinations of parameters, excluding the phi 
	parameters estimated in the beta-binomial model.
}
\usage{
plot_all_joint_marginals(MCMC.output, percent.burnin = 0, thinning = 1)
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
}
\details{
	Visualizations of the joint marginal distributions allow users to (1) assess how well 
	the MCMC is mixing, and (2) potentially diagnose instances of non-identifiability in
	the model.  Strong linear trends in the joint marginal, or visible "ridges" in the 
	likelihood surface, may be indicative of parameter non-identifiability, in which 
	multiple combinations of values of these two parameters provide equally reasonable 
	fits to the data.
}
\author{
Gideon Bradburd
}