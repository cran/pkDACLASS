\name{Denoising.R}
\alias{Denoising.R}
\title{
Denoising mass spectrometry data
}
\description{
The function Denoising.R takes as input, baseline corrected data and returns a denoised data. It implements a method to assure no change of the intensities (y) of the remaining features m/z. It further maintains the m/z values with corresponding y greater than a certain specified threshold (h).
}
\usage{
Denoising.R(dat, h, ...)
}
\arguments{
  \item{dat}{
Baseline corrected data: a dataframe of two columns: the first column being the m/z values, and the second column their corresponding baseline corrected spectra intensities.
}
  \item{h}{
Height, threshold value where to cut the intensities.
}
  \item{\dots}{
Additional arguments to pass to the denoising function.
}
}
\details{
Given a noisy raw data y; a cuttoff point (h) is chosen such that the features selected correspond to real m/z peaks. The cutoff chosen should be large enough to eliminate the initial noisy region but small enough to retain any peaks that could correspond to real observable proteins or peptides. The principle is based on keeping the features with intensities greater than a certain threshold (h). In other words, consider only values of y that are greater than h and 0, otherwise.
}
\value{
  \item{m.z }{
denoised mass to charge ratios.
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

