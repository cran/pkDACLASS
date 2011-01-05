\name{dataGen}
\alias{dataGen}
\title{
Generating a Location-shifted Poisson Distribution
}
\description{
The function dataGen generates location-shifted Poisson random variates. 
}
\usage{
dataGen(n, lambda1, lambda2, w, ...)
}
\arguments{
  \item{n}{
Integer, sample size.
}
  \item{lambda1}{
Numeric parameter for the first Poisson distribution.
}
  \item{lambda2}{
Numeric parameter for the second Poisson distribution.
}
  \item{w}{
Numeric parameter, between 0 and 1, which determines if the random variate generated comes from the first Poisson model or the second Poisson model. Sometimes called the mixing proportion.
}
  \item{\dots}{
Additional arguments to pass to the random variate generating function.
}
}
\details{
The function dataGen generates random variates for the location-shifted Poisson model, with w proportion from the first Poisson distribution and (1-w) from the second Poisson model.
}
\value{
A matrix of cluster of features is returned. Each cluster consists of two columns; the first of which are the mass to charge values and the second their corresponding intensities.
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

