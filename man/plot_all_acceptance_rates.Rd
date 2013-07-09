\name{plot_all_acceptance_rates}
\alias{plot_all_acceptance_rates}
\title{
	Plots the acceptance rates of all parameters across MCMC generations
}
\description{
	Creates a series of plots showing the proportion of proposed moves to accepted moves 
	over the duration of the MCMC analysis for each parameter updated via a random-walk
	sampler. 
}
\usage{
plot_all_acceptance_rates(MCMC.output)
}
\arguments{
  \item{MCMC.output}{
	The standard MCMC output file generated from a BEDASSLE run.
}
}
\details{
	For optimal mixing, between ~20% and ~70% of the moves proposed by the random-walk
	samplers should be accepted.  If the acceptance rates fall outside that range, this 
	function will automatically highlight that parameter as a potential instance of poor
	mixing.  If the acceptance rates are too low, then for subsequent analyses the user 
	should \emph{decrease} the scale of the tuning parameter (or "std," as in, e.g., 
	\code{aD_std}), and if acceptance rates are too high, the user should \emph{increase} 
	the scale of the tuning parameter.  The scale of the tuning parameter is the standard 
	deviation of the normal distribution from which the small random variable is drawn 
	and added to the current parameter value to propose a move.   If the acceptance rate 
	has not plateaued by the end of an analysis, it is an indication that the chain may 
	still be "going somewhere" in parameter space, and subsequent analyses should be 
	performed.
}
\author{
Gideon Bradburd
}