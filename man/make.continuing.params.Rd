\name{make.continuing.params}
\alias{make.continuing.params}
\title{
	Generates an R object containing the last parameter values of an MCMC run (to be 
	used for a subsequent run)
}
\description{
	This function creates an R object that contains the parameter values read from the 
	last generation of a previous MCMC run.  This R object can then be used to initiate
	a subsequent analysis, effectively creating a single long chain.  [A single MCMC
	object from both runs can be created using the function 
	\command{link.up.posteriors}].
}
\usage{
make.continuing.params(MCMC.output, file.name)
}
\arguments{
  \item{MCMC.output}{
	The standard MCMC output file generated from a BEDASSLE run.
}
  \item{file.name}{
	The user-defined name assigned to the R object of parameters to be used in continuing 
	analysis.
}
}
\author{
Gideon Bradburd
}