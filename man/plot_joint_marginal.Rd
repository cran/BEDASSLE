\name{plot_joint_marginal}
\alias{plot_joint_marginal}
\title{
	Plots the joint marginal for a pair of parameters
}
\description{
	For each sampled MCMC generation, the values estimated for a pair of parameters are 
	logged and plotted against one another.  Points are color coded by when in the 
	analysis they were sampled, so that users can visually assess mixing.
}
\usage{
plot_joint_marginal(parameter1, parameter2, percent.burnin = 0, thinning = 1, 
param.name1 = deparse(substitute(parameter1)), 
param.name2 = deparse(substitute(parameter2)))
}
\arguments{
  \item{parameter1}{
	One of the two parameters for which the joint marginal is being plotted.
}
  \item{parameter2}{
	The other of the two parameters for which the joint marginal is being plotted.
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
  \item{param.name1}{
	The name of one of the two parameters for which the joint marginal is being plotted.
}
  \item{param.name2}{
	The name of the other of the two parameters for which the joint marginal is being 
	plotted.
}
}
\details{
	Visualizations of the joint marginal distribution allow users to (1) assess how well 
	the MCMC is mixing, and (2) potentially diagnose instances of non-identifiability in
	the model.  Strong linear trends in the joint marginal, or visible "ridges" in the 
	likelihood surface, may be indicative of parameter non-identifiability, in which 
	multiple combinations of values of these two parameters provide equally reasonable 
	fits to the data.
}
\author{
Gideon Bradburd
}