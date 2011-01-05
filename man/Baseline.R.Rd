\name{Baseline.R}
\alias{Baseline.R}
\title{
Baseline Correction of Mass Spectrometry Data
}
\description{
The function Baseline.R returns data that is baseline corrected with a common scale and fair homoscedasticity. The routine, bslnoff, in the PROcess library is used to remove baseline drift from the spectrum. The function bslnoff divides the spectrum into unequal sections, finds a minimum or quantile corresponding to a given probability of each section, replace each intensity by that minimum and fits a curve through all points.
}
\usage{
Baseline.R(dat, w, p = TRUE, ...)
}
\arguments{
  \item{dat}{
Real data, a dataframe with two columns for which the first column are the m/z values and the corresponding intensities in the second column.
}
  \item{w}{
The bandwidth to be passed to "approx" method, for linear interpolation. The default is set to 0.0251.
}
  \item{p}{
Logical; TRUE or FALSE, if TRUE it will return the raw spectrum, the estimated baseline and the baseline subtracted spectrum. The default is set to TRUE.
}
  \item{\dots}{
Additional arguments to pass to the baseline function.
}
}
\details{
Baseline subtraction uses an iterative algorithm to attempt to remove the baseline slope and offset from a spectrum by interactively calculating the best-fit straight line through a set of estimated baseline points. The baseline correction relies on a method that has been implemented in PROcess package. The baseline is subtracted by setting the bandwidth of "approx" method in the routine bslnoff, to be 25%. The baseline is subtracted from the spectrum for each point and all spectra intensity is rescaled to positive producing a baseline corrected spectrum.
}
\value{
  \item{m.z }{
baseline corrected mass to charge ratios.
}
  \item{y }{
corresponding intensities.
}
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf

Li, X. (2001) PROcess R Library by Xiaochun Li/R 2.1.1 http://www.bioconductor.org
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

