\name{BEDASSLE-package}
\alias{BEDASSLE-package}
\alias{BEDASSLE}
\docType{package}
\title{
Disentangling the contributions of geographic and ecological isolation to genetic 
differentiation
}
\description{
This method models the covariance in allele frequencies between populations on a 
landscape as a decreasing function of their pairwise geographic and ecological distance.  
Allele frequencies are modeled as a spatial Gaussian process with a parametric covariance 
function. The parameters of this covariance function, as well as the spatially smoothed 
allele frequencies, are estimated in a custom Markov chain Monte Carlo.

The two inference functions are \code{MCMC} and \code{MCMC_BB}, which call the 
Markov chain Monte Carlo algorithms on the standard and overdispersion (Beta-Binomial)
models, respectively.  To evaluate MCMC performance, there are a number of MCMC diagnosis
and visualization functions, which variously show the trace, plots, marginal and joint
marginal densities, and parameter acceptance rates.  To evaluate model adequacy, there is 
a posterior predictive sample function (\code{posterior.predictive.sample}), and an
accompanying function to plot its output and visually assess the model's ability to 
describe the user's data.  
}
\author{
Gideon Bradburd

Maintainer: 
Gideon Bradburd <gbradburd@ucdavis.edu>
}
\references{
Bradburd, G.S., Ralph, P.L., and Coop, G.M. Disentangling the effects of geographic and 
ecological isolation on genetic differentiation. \emph{Evolution} 2013.
}

