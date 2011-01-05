\name{mixtureEM.R}
\alias{mixtureEM.R}
\title{
EM algorithm for Estimating Parameters of the Location-Shifted Poisson Model
}
\description{
The function mixtureEM.R returns the parameters of the location-shifted Poisson fit. The parameters are w, the mixing proportion, between o and 1, lambda1 from the first Poisson model and lambda2 from the second Poisson model.
}
\usage{
mixtureEM.R(dat)
}
\arguments{
  \item{dat}{
A cluster of features.
}
}
\details{
The function mixtureEM.R fits a location-shifted Poisson model to the data, and estimates the parameters by the EM algorithm. The EM algorithm is used to determine from which model the monoisotopic peak comes from i.e. from the first Poisson model or the second one.
}
\value{
The function returns a list of parameter estimates and the starting values.
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf

Dempster AP, Laird NM, Rubin DB (1997) Maximum Likelihood from incomplete data via the EM algorithm, Journal of the Royal Statistical Soceity. Series B: Methodology 39:1-22.
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

