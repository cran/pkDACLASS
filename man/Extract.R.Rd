\name{Extract.R}
\alias{Extract.R}
\title{
Extracting the Isotopic Distribution
}
\description{
The function Extract.R takes as input, binned data and returns a set of cluster of features and their corresponding intensities. Explicit knowledge of chemistry of the peptide molecules is used to identify the isotopic distributions.
}
\usage{
Extract.R(dat, ...)
}
\arguments{
  \item{dat}{
Binned data, dataframe of two columns; column one are the m/z values and column two their corresponding intensities.
}
  \item{\dots}{
Additional arguments to pass on to the extraction function.
}
}
\details{
Assume that contiguous peaks x or m/z of 1Da apart exist in an isotopic distribution and that a is taken as the starting value for identifying an isotopic distribution pattern in a spectrum. Check that there are no peaks to the left of a within 1-0.05Da and 1+0.05Da, where 0.05 is the alignment error due to limitations of mass resolution. An isotopic distribution can be identified by selecting the peaks at a, a+1(-0.05,+0.05), a+2(-.05,+0.05),...; stop if a gap exists. The procedure is repeated until all possible isotopic distribution patterns in the spectrum is formed.
}
\value{
A set of cluster of features. Each set is a dataframe of two columns, the first column are m/z values and the second column their corresponding intensities.
}
\references{
Atlas, M and Datta, S (2009) A Statistical Technique for Monoisotopic Peak Detection in a Mass Spectrum. J Proteomics Bioinform 2:202-216 http://www.omicsonline.com/ArchiveJPB/2009/May/01/JPB2.202.pdf
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

