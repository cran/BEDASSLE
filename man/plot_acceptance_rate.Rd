\name{plot_acceptance_rate}
\alias{plot_acceptance_rate}
\title{
	Plots the acceptance rate of a parameter across MCMC generations
}
\description{
	Creates a plot showing the proportion of proposed moves to accepted moves over the 
	duration of the MCMC analysis. 
}
\usage{
plot_acceptance_rate(accepted.moves, proposed.moves, param.name =
deparse(substitute(accepted.moves)))
}
\arguments{
  \item{accepted.moves}{
	A vector giving the number of accepted random-walk moves at each sampled MCMC 
	generation.
}
  \item{proposed.moves}{
	A vector giving the number of proposed random-walk moves at each sampled MCMC 
	generation.
}
  \item{param.name}{
	The name of the parameter for which the trace plot is being displayed.
}
}
\details{
	For optimal mixing, between ~20% and ~70% of the moves proposed by the random-walk
	samplers should be accepted.  If the acceptance rates fall outside that range, this 
	function will automatically highlight that parameter as a potential instance of poor
	mixing.  If the acceptance rates are too low, then for subsequent analyses the user 
	should \emph{decrease} the scale of the tuning parameter (or "std," as in, e.g., 
	"aD_std"), and if acceptance rates are too high, the user should \emph{increase} the 
	scale of the tuning parameter.  The scale of the tuning parameter is the standard 
	deviation of the normal distribution from which the small random variable is drawn 
	and added to the current parameter value to propose a move.  If the acceptance rate 
	has not plateaued by the end of an analysis, it is an indication that the chain may 
	still be "going somewhere" in parameter space, and subsequent analyses should be 
	performed.
}
\author{
Gideon Bradburd
}