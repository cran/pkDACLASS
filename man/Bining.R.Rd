\name{Bining.R}
\alias{Bining.R}
\title{
Binning the data
}
\description{
The function Bining.R returns a binned data with one Dalton between features. Once denoising is done, data dimensionality reduction is performed by grouping the mass to charge (m/z) values into bins. Binning is applied to divide the m/z axis into intervals of desired length, which will assist in the extraction of meaningful isotopic patterns.
}
\usage{
Bining.R(dat, INDEX, ...)
}
\arguments{
  \item{dat}{
Baseline corrected and denoised data. A dataframe of two columns, the first column being the m/z values and the second column the corresponding intensities.
}
  \item{INDEX}{
Rounds mass to charge values
}
  \item{\dots}{
Additional arguments to pass through the bining function.
}
}
\details{
The binning scheme works as follows; Start by rounding all mass to charge values. Then, moving from the lowest rounded m/z value of the spectrum to the right along the m/z axis, for each rounded m/z values say b, search for the maximum intensity y within 1Da interval (b-0.5, b+0.5). Further, maintain the m/z value corresponding to the maximum value of the intensity y.
}
\value{
  \item{m.z }{
binned mass to charge ratios.
}
  \item{y }{
corresponding intensities.
}
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

