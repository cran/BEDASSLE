\name{link.up.posteriors}
\alias{link.up.posteriors}
\title{
	Links up multiple MCMC output objects
}
\description{
	Creates a single MCMC output object that links together the output from 2 different
	runs.  To be used when analyses are run serially on a single dataset, with subsequent
	runs initiated at the parameter values estimated in the last generation of the 
	previous MCMC run.  
}
\usage{
link.up.posteriors(MCMC.output1, MCMC.output2, linked.up.output.file.name)
}
\arguments{
  \item{MCMC.output1}{
	The first (chronologically) MCMC output to be incorporated into the linked-up output
	object.
}
  \item{MCMC.output2}{
	The second (chronologically) MCMC output to be incorporated into the linked-up output
	object.
}
  \item{linked.up.output.file.name}{
	The linked-up MCMC object file name.  The suffix ".Robj" will be appended to the 
	user-specified name.
}
}
\details{
	Acceptance rates are re-calculated to be consistent across the new, larger MCMC 
	object. The function is also flexible with respect to the model parameterization 
	(e.g. - it will recognize, for example, whether users have specified the standard or 
	beta-binomial models, or whether users have specified one, or more than one, alphaE 
	parameters).
}
\author{
Gideon Bradburd
}